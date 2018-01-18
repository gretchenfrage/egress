package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentLinkedQueue, Executors, ThreadLocalRandom, ThreadPoolExecutor}

import com.phoenixkahlo.hellcraft.core.client.{ClientRenderWorld, ClientWorld, WorldBounds}
import com.phoenixkahlo.hellcraft.core.entity.{AnyEnt, AnyEntID, EntID, Entity}
import com.phoenixkahlo.hellcraft.core.eval.{AsyncEval, WEval}
import com.phoenixkahlo.hellcraft.core.event.UE._
import com.phoenixkahlo.hellcraft.core.event.UE
import com.phoenixkahlo.hellcraft.core.request.{Request, Requested}
import com.phoenixkahlo.hellcraft.core.util.GroupedEffects
import com.phoenixkahlo.hellcraft.core.{CallService, Chunk, Event, EventID, IdenEvent, LogEffect, MakeRequest, PutChunk, PutEnt, RemEnt, SoundEffect, Terrain, UpdateEffect, event}
import com.phoenixkahlo.hellcraft.math.{MRNG, V3I}
import com.phoenixkahlo.hellcraft.service.procedures.PhysicsServiceProcedure
import com.phoenixkahlo.hellcraft.service.{Service, ServiceProcedure, ServiceTagTable}
import com.phoenixkahlo.hellcraft.singleplayer.AsyncSave.GetPos
import com.phoenixkahlo.hellcraft.singleplayer.SingleContinuum.IncompleteKey
import com.phoenixkahlo.hellcraft.singleplayer.SingleWorld.{apply => _, _}
import com.phoenixkahlo.hellcraft.util.collections.{BBox, ParGenMutHashMap, V3ISet}
import com.phoenixkahlo.hellcraft.util.debugging.Profiler
import com.phoenixkahlo.hellcraft.util.threading._
import com.phoenixkahlo.hellcraft.util.time.Timer

import scala.concurrent.duration._
import scala.collection.{JavaConverters, mutable, parallel}
import scala.util.Random

case class SingleWorld(
                      chunks: Map[V3I, Either[ChunkEnts, Terrain]],
                      ents: Map[AnyEntID, V3I],
                      cdomain: V3ISet,
                      tdomain: V3ISet,
                      active: Set[V3I],
                      renderable: Set[V3I],
                      bbox: BBox
                      ) extends ClientWorld {
  // chunk lookup
  override def chunk(p: V3I): Option[Chunk] =
    chunks.get(p) match {
      case Some(Left(ChunkEnts(chunk, _))) => Some(chunk)
      case _ => None
    }

  // chunk and ents lookup
  def chunkEnts(p: V3I): Option[ChunkEnts] =
    chunks.get(p).flatMap(_.left.toOption)

  // terrain lookup
  override def terrain(p: V3I): Option[Terrain] =
    chunks.get(p).map({
      case Left(ChunkEnts(chunk, _)) => chunk.terrain
      case Right(terrain) => terrain
    })

  // entity lookup
  override def findEntity[E <: Entity[E]](id: EntID[E]): Option[E] =
    ents.get(id).flatMap(p => chunks(p).left.get.ents.get(id).asInstanceOf[Option[E]])

  // untyped entity lookup
  def findEntityUntyped(id: AnyEntID): Option[AnyEnt] =
    ents.get(id).flatMap(p => chunks(p).left.get.ents.get(id))

  // bounding box
  override def bounds: WorldBounds = bbox()

  // loaded chunks
  override def loadedChunks: Seq[V3I] =
    chunks.keySet.toSeq.filter(chunks(_).isLeft)

  // loaded terrains
  override def loadedTerrain: Seq[V3I] =
    chunks.keySet.toSeq

  // extend world to renderable version
  def renderable(_ftime: Float, _interp: Float): ClientRenderWorld =
    new SingleWorld(chunks, ents, cdomain, tdomain, active, renderable, bbox) with ClientRenderWorld {
      override def renderableChunks = renderable.toSeq.map(chunks).map(_.left.get.chunk)

      override def renderableEnts = active.toSeq.map(chunks).flatMap(_.left.get.ents.values)

      override def ftime = _ftime

      override def interp = _interp
    }

  // remove chunks/terrains, return removed chunk
  def --(ps: Seq[V3I]): (SingleWorld, Seq[Chunk]) = {
    val rc = ps.flatMap(chunks.get).flatMap(_.left.toOption)
    val es = rc.flatMap(_.ents.keySet)
    (
      SingleWorld(chunks -- ps, ents -- es, cdomain, tdomain, active -- ps, renderable -- ps, bbox -- ps),
      rc.map(_.chunk)
    )
  }

  // remove chunk/terrain, return removed chunk
  def -(p: V3I): (SingleWorld, Option[Chunk]) = {
    val es = chunks.get(p).flatMap(_.left.toOption).toSeq.flatMap(_.ents.keySet)
    (SingleWorld(chunks - p, ents -- es, cdomain, tdomain, active - p, renderable - p, bbox - p), chunk(p))
  }

  // downgrade chunks to terrains, return removed chunks
  def downgrade(ps: Seq[V3I]): (SingleWorld, Seq[Chunk]) = {
    val cs = ps.flatMap(chunks.get).flatMap(_.left.toOption)
    val es = cs.flatMap(_.ents.keySet)
    (
      copy(chunks = ps.foldLeft(chunks)({
        case (map, p) => map.get(p) match {
          case Some(Left(ChunkEnts(chunk, _))) => map.updated(p, Right(chunk.terrain))
          case _ => map
        }
      }), ents = ents -- es, active = active -- ps, renderable = renderable -- ps, bbox = bbox -- ps),
      cs.map(_.chunk)
    )
  }

  // put chunks in world, return replace chunk pairs
  def putChunks(cs: Seq[Chunk]): (SingleWorld, Seq[ChunkExchange]) = {
    if (cs.isEmpty) (this, Seq.empty)
    else {
      val (newChunks, replaced) = cs.foldLeft((chunks, Seq.empty[ChunkExchange]))({
        case ((map, replaced), chunk) => map.get(chunk.pos) match {
          case Some(Left(ChunkEnts(c, e))) =>
            (map.updated(chunk.pos, Left(ChunkEnts(chunk, e))), replaced :+ ChunkExchange(Some(c), Some(chunk)))
          case _ =>
            (map.updated(chunk.pos, Left(ChunkEnts(chunk, Map.empty))), replaced)
        }
      })
      (
        copy(
          chunks = newChunks,
          renderable = renderable ++ cs.filter(_.isRenderable).map(_.pos),
          bbox = bbox ++ cs.map(_.pos)
        ),
        replaced
      )
    }
  }

  // put terrains in world
  def putTerrains(ts: Seq[Terrain]): (SingleWorld, Seq[ChunkExchange]) = {
    if (ts.isEmpty) (this, Seq.empty)
    else {
      val (newChunks, replaced) = ts.foldLeft((chunks, Seq.empty[ChunkExchange]))({
        case ((map, replaced), terrain) => map.get(terrain.pos) match {
          case Some(Left(ChunkEnts(c, e))) =>
            (map.updated(terrain.pos, Right(terrain)), replaced :+ ChunkExchange(Some(c), None))
          case _ =>
            (map.updated(terrain.pos, Right(terrain)), replaced)
        }
      })
      (
        copy(
          chunks = newChunks,
          renderable = renderable -- ts.map(_.pos),
          bbox = bbox -- ts.map(_.pos)
        ),
        replaced
      )
    }
  }

  // put entity in world without removing it first
  // assumes the entity doesn't exist and will glitch if it does
  private def _putEntUnsafe(ent: AnyEnt): (SingleWorld, EntExchange) = {
    chunks.get(ent.chunkPos) match {
      case Some(Left(ChunkEnts(chunk, entMap))) =>
        (copy(
          chunks = chunks.updated(ent.chunkPos, Left(ChunkEnts(chunk, entMap + (ent.id -> ent)))),
          ents = ents + (ent.id -> ent.chunkPos),
          active = active + ent.chunkPos,
          renderable = renderable + ent.chunkPos
        ), EntExchange(None, Some(ent)))
      case _ =>
        println("warn: failed to put ent, chunk non-existent")
        (this, EntExchange.nill)
    }
  }

  // update the entity by removing then putting
  def putEnt(ent: AnyEnt): (SingleWorld, EntExchange) = {
    val (w0, e0) = this remEnt ent.id
    val (w1, e1) = w0 _putEntUnsafe ent
    (w1, e0 andThen e1)
    //remEnt(ent.id)._putEnt(ent)
  }

  // put a sequence of chunk/ents
  def putChunkEnts(chunkEnts: Seq[ChunkEnts]): (SingleWorld, Seq[ChunkExchange], Seq[EntExchange]) = {
    val (w0, ce) = this putChunks chunkEnts.map(_.chunk)
    val (w1, ee) = chunkEnts.flatMap(_.ents.values).foldLeft((w0, Seq.empty[EntExchange]))({
      case ((world, accum), ent) =>
        val (w1, ee) = world putEnt ent
        (world, accum :+ ee)
    })
    (w1, ce, ee)
  }

  // remove entity from world
  def remEnt(id: AnyEntID): (SingleWorld, EntExchange) = {
    ents.get(id).map(p => chunks.get(p) match {
      case Some(Left(ChunkEnts(chunk, ents))) =>
        ents.get(id) match {
          case Some(ent) =>
            (copy(
              chunks = chunks.updated(p, Left(ChunkEnts(chunk, ents - id))),
              active = if (ents.size == 1) active - p else active,
              renderable = if (!chunk.isRenderable) renderable - p else renderable
            ), EntExchange(Some(ent), None))
          case None =>
            println("warn: failed to remove ent, not present in chunk")
            (this, EntExchange.nill)

        }
      case _ =>
        println("warn: failed to remove ent, chunk non-existent")
        (this, EntExchange.nill)
    }).getOrElse({
      //println("warn: failed to remove ent, not found")
      (this, EntExchange.nill)
    })
  }

  // update the domain
  def setDomain(_cdomain: V3ISet, _tdomain: V3ISet): SingleWorld =
    copy(cdomain = _cdomain, tdomain = _tdomain)

  // evaluate a UE monad
  def eval[T](ue: UE[T], time: Long): T = ue match {
    case ue@UEGen(fac) => fac()
    case ue@UEMap(src, func) =>
      def f[S, T](ue: UEMap[S, T]): T = ue.func(eval(ue.src, time))
      f(ue)
    case ue@UEFMap(src, func) =>
      def f[S, T](ue: UEFMap[S, T]): T = eval(ue.func(eval(ue.src, time)), time)
      f(ue)
    case ue@UEFilter(src: UE[T], test: (T => Boolean)) =>
      val t: T = eval(src, time)
      if (!test(t))
        println("warn: UE filter monad failed filter (ignoring)")
      t
    case ue@UEChunk(p) => chunk(p).asInstanceOf[T]
    case ue@UETerrain(p) => terrain(p).asInstanceOf[T]
    case ue@UEEnt(id) => findEntityUntyped(id).asInstanceOf[T]
    case UETime => time.asInstanceOf[T]
    case UERand => new MRNG(ThreadLocalRandom.current.nextLong()).asInstanceOf[T]
  }

  // update to a new world, and get output effects, and a summary of changed chunks and entites
  def update(_time: Long, externs: Seq[UpdateEffect], services: ServiceTagTable[ServiceProcedure]): (SingleWorld, Seq[UpdateEffect], ChangeSummary) = {
    val queue: java.util.Deque[UpdateEffect] = new java.util.LinkedList

    externs.foreach(queue.add)
    active.toSeq.flatMap(chunks(_).left.get.ents.values).flatMap(_.update).foreach(queue.add)

    var world = this
    val outEffects = new mutable.ArrayBuffer[UpdateEffect]
    val chunkExchanges = new mutable.ArrayBuffer[ChunkExchange]
    val entExhanges = new mutable.ArrayBuffer[EntExchange]

    while (!queue.isEmpty) queue.remove() match {
      case effect: SoundEffect => outEffects += effect
      case effect: MakeRequest[_] => outEffects += effect
      case effect: LogEffect => outEffects += effect
      case effect@PutChunk(chunk) =>
        if (world.cdomain contains chunk.pos) {
          val (w, r) = world putChunks Seq(chunk)
          world = w
          chunkExchanges ++= r
        } else outEffects += effect
      case effect@PutEnt(ent) =>
        if (world.chunk(ent.chunkPos).isDefined) {
          val (w, e) = world putEnt ent
          world = w
          entExhanges += e
        } else outEffects += effect
      case effect@RemEnt(id) =>
        if (world.findEntityUntyped(id).isDefined) {
          val (w, e) = world remEnt id
          world = w
          entExhanges += e
        } else outEffects += effect
      case effect: CallService[_, _] =>
        def f[S <: Service, T](call: CallService[S, T]): Seq[UpdateEffect] = {
          world.eval(call.io, _time) match {
            case Some((invoke: S#Call[T], onComplete: (T => Event))) =>
              val o: Event = onComplete(PartialSyncEval(exec => world.eval(services(call.service).apply(invoke)(exec), _time)))
              world.eval(o.eval, _time)
            case None => Seq.empty
          }
        }
        f(effect).reverse.foreach(queue.addFirst)
      case effect@Event(ue) =>
        world.eval(ue, _time).reverse.foreach(queue.addFirst)
      case effect: IdenEvent => ???
    }

    (world, outEffects, ChangeSummary(chunkExchanges, entExhanges))
  }

  def parUpdate(time: Long, externs: Seq[UpdateEffect], services: ServiceTagTable[ServiceProcedure]): (SingleWorld, Seq[UpdateEffect], ChangeSummary) = {
    val exec: Runnable => Unit = SingleWorld.parUpdatePool.execute

    object State {
      private val chunks = new mutable.HashMap[V3I, Option[Fut[Chunk]]].withDefault(
        p => SingleWorld.this.chunk(p).map(chunk => Fut(chunk, _.run()))
      )
      private val ents = new mutable.HashMap[AnyEntID, Fut[Option[AnyEnt]]].withDefault(
        id => Fut(SingleWorld.this.findEntityUntyped(id), _.run())
      )
      private val out = new mutable.ArrayBuffer[UpdateEffect]
      private val lock = new FutSequence(exec)

      type ChunkMap = mutable.Map[V3I, Option[Fut[Chunk]]]
      type EntMap = mutable.Map[AnyEntID, Fut[Option[AnyEnt]]]
      type Mutator[R] = (ChunkMap, EntMap, mutable.Buffer[UpdateEffect]) => R
      def mutate[R](mutator: Mutator[R]): Fut[R] = lock.apply[R](() => mutator(chunks, ents, out))

      def chunkPuts: Seq[Chunk] = chunks.values.toSeq.flatten.map(_.await)
      def entPuts: Seq[(AnyEntID, Option[AnyEnt])] = ents.toSeq.map({ case (id, fut) => (id, fut.await) })
      def outEffects: Seq[UpdateEffect] = out
    }

    sealed trait StateAcquire
    case class ChunkAcquire(p: V3I, before: Fut[Chunk], callback: SettableFut[Chunk]) extends StateAcquire
    case class EntAcquire(id: AnyEntID, before: Fut[Option[AnyEnt]], callback: SettableFut[Option[AnyEnt]]) extends StateAcquire

    case class AltSources(chunks: Map[V3I, Fut[Chunk]], ents: Map[AnyEntID, Fut[Option[AnyEnt]]])
    object AltSources {
      def fromAcquired(acq: Seq[StateAcquire]): AltSources = {
        val (c, e) = acq.partition(_.isInstanceOf[ChunkAcquire])
        AltSources(
          c.asInstanceOf[Seq[ChunkAcquire]].map(acq => (acq.p, acq.before)).toMap,
          e.asInstanceOf[Seq[EntAcquire]].map(acq => (acq.id, acq.before)).toMap
        )
      }
    }
    object NoAlts extends AltSources(Map.empty, Map.empty)

    def evaluate[T, R](ue: UE[T], onComplete: (T, Seq[StateAcquire]) => R)(implicit alts: AltSources = NoAlts): Fut[R] = ue match {
      case ue@UEGen(fac) => Fut(onComplete(fac(), Seq.empty), exec)
      case ue@UEMap(src, func) =>
        def f[S, T, R](ue: UEMap[S, T], onComplete: (T, Seq[StateAcquire]) => R): Fut[R] =
          evaluate(ue.src, (s: S, acq) => Fut(onComplete(ue.func(s), acq), exec)).flatten
        f(ue, onComplete)
      case ue@UEFMap(src, func) =>
        def f[S, T, R](ue: UEFMap[S, T], onComplete: (T, Seq[StateAcquire]) => R): Fut[R] =
          evaluate(ue.src, (s: S, acq1) => Fut(evaluate(ue.func(s), (t: T, acq2) => Fut(onComplete(t, acq1 ++ acq2), exec)), exec)).flatten.flatten.flatten
        f(ue, onComplete)
      case ue@UEFilter(src: UE[T], test: (T => Boolean)) =>
        evaluate(src, (t: T, acq) => {
          if (!test(t))
            println("warn: UE filter monad failed filter (ignoring)")
          onComplete(t, acq)
        })
      case ue@UEChunk(p) =>
        alts.chunks.get(p).map(_.map(Some(_).asInstanceOf[R])).getOrElse({
          State.mutate[Fut[R]]((chunks, ents, out) => {
            chunks(p) match {
              case Some(before: Fut[Chunk]) =>
                val callback = new SettableFut[Chunk]
                val acquire = ChunkAcquire(p, before, callback)
                chunks(p) = Some(callback)
                before.map((chunk: Chunk) => onComplete(Some(chunk).asInstanceOf[T], Seq(acquire)))
              case None =>
                Fut(onComplete(None.asInstanceOf[T], Seq.empty), exec)
            }
          }).flatten
        })
      case ue@UETerrain(p) =>
        alts.chunks.get(p).map(_.map(chunk => Some(chunk.terrain).asInstanceOf[R])).getOrElse({
          State.mutate[Fut[R]]((chunks, ents, out) => {
            chunks(p) match {
              case Some(fut: Fut[Chunk]) => fut.map[R]((any: Chunk) => onComplete(Some(fut.query.get.terrain).asInstanceOf[T], Seq.empty), exec)
              case None => Fut[R](onComplete(SingleWorld.this.terrain(p).asInstanceOf[T], Seq.empty), exec)
            }
          }).flatten
        })
      case ue@UEEnt(id) =>
        alts.ents.get(id).map(_.asInstanceOf[Fut[R]]).getOrElse({
          State.mutate((chunks, ents, out) => {
            val before: Fut[Option[AnyEnt]] = ents(id)
            val callback: SettableFut[Option[AnyEnt]] = new SettableFut[Option[AnyEnt]]
            val acquire = EntAcquire(id, before, callback)
            ents(id) = callback
            before.map(ent => onComplete(ent.asInstanceOf[T], Seq(acquire)), exec)
          }).flatten
        })
      case UETime => Fut(onComplete(time, Seq.empty), exec)
      case UERand => Fut(onComplete(new MRNG(ThreadLocalRandom.current.nextLong()).asInstanceOf[T], Seq.empty), exec)
    }

    def coherentProcess(caused: Seq[UpdateEffect], acquired: Seq[StateAcquire]): Promise = {
      val acqChunks: Set[V3I] = acquired.flatMap({
        case ChunkAcquire(p, _, _) => Some(p)
        case _ => None
      }).toSet
      val acqEnts: Set[AnyEntID] = acquired.flatMap({
        case EntAcquire(id, _, _) => Some(id): Option[AnyEntID]
        case _ => None: Option[AnyEntID]
      }).toSet

      val cputNow = new mutable.HashMap[V3I, Chunk]
      val eputNow = new mutable.HashMap[AnyEntID, Option[AnyEnt]]
      val causeAsync = new mutable.ArrayBuffer[UpdateEffect]

      caused foreach {
        case PutChunk(chunk) if acqChunks contains chunk.pos =>
          cputNow(chunk.pos) = chunk
        case PutEnt(ent) if acqEnts contains ent.id =>
          eputNow(ent.id) = Some(ent)
        case RemEnt(id) if acqEnts contains id =>
          eputNow(id) = None
        case other => causeAsync += other
      }

      acquired foreach {
        case ChunkAcquire(p, before, callback) => cputNow.get(p) match {
          case Some(chunk) => callback.set(chunk)
          case None => callback.observe(before)
        }
        case EntAcquire(id, before, callback) => eputNow.get(id) match {
          case Some(ent) => callback.set(ent)
          case None => callback.observe(before)
        }
      }

      PromiseFold(causeAsync.map(process))
    }

    def process(effect: UpdateEffect): Promise =
      Fut[Promise](effect match {
        case effect: SoundEffect => State.mutate[Unit]((chunks, ents, out) => {
          //println("sound effect")
          out += effect
        })
        case effect: MakeRequest[_] => State.mutate[Unit]((chunks, ents, out) => {
          //println("make request")
          out += effect
        })
        case effect: LogEffect => State.mutate[Unit]((chunks, ents, out) => {
          //println("log effect")
          out += effect
        })
        case effect@PutChunk(chunk) => State.mutate[Unit]((chunks, ents, out) => {
          //println("put chunk")
          if (cdomain contains chunk.pos)
            chunks(chunk.pos) = Some(Fut(chunk, _.run()))
          else
            out += effect
        })
        case effect@PutEnt(ent) => State.mutate[Unit]((chunks, ents, out) => {
          //println("put ent " + ent)
          if (chunks(ent.chunkPos).isDefined)
            ents(ent.id) = Fut(Some(ent), _.run())
          else
            out += effect
        })
        case effect@RemEnt(id) => State.mutate[Unit]((chunks, ents, out) => {
          //println("rem ent")
          if (ents contains id)
            ents(id) = Fut(None, _.run())
          else
            out += effect
        })
        case effect@Event(ue) => evaluate[Seq[UpdateEffect], Fut[Unit]](
          ue, (caused: Seq[UpdateEffect], acquired: Seq[StateAcquire]) => {
            //println("processing event result " + caused)
            coherentProcess(caused, acquired)
          }).flatten
        case effect: CallService[_, _] =>
          def f[S <: Service, T](call: CallService[S, T]): Promise = {
            evaluate[Option[(S#Call[T], T => Event)], Promise](call.io, (opt, acq1) => opt match {
              case Some((invoke, onComplete)) =>
                evaluate[Fut[T], Promise](services(call.service).apply(invoke)(exec),
                  (fut, acq2) => {
                    fut.map(onComplete, exec).map[Promise](
                      (event: Event) => {
                        evaluate[Seq[UpdateEffect], Promise](event.eval,
                          (caused, acq3) => {
                            coherentProcess(caused, acq1 ++ acq2 ++ acq3)
                          })(AltSources.fromAcquired(acq1 ++ acq2)).flatten
                      }).flatten
                  })(AltSources.fromAcquired(acq1)).flatten
              case None => Promise.nothing
            }).flatten
          }
          f(effect)
      }, exec).flatten

    val begin: Seq[UpdateEffect] = externs ++ active.toSeq.flatMap(chunks(_).left.get.ents.values).flatMap(_.update)
    val completion: Promise = PromiseFold(begin.map(process))

    completion.await

    var world = this
    val entExhanges = new mutable.ArrayBuffer[EntExchange]
    val (w, chunkExchanges) = world putChunks State.chunkPuts
    world = w
    State.entPuts foreach {
      case (id, Some(ent)) =>
        val (w, e) = world putEnt ent
        world = w
        entExhanges += e
      case (id, None) =>
        val (w, e) = world remEnt id
        world = w
        entExhanges += e
    }

    (world, State.outEffects, ChangeSummary(chunkExchanges, entExhanges))
  }

}
object SingleWorld {
  lazy val parUpdatePool = Executors.newFixedThreadPool(4, task => {
    val thread = new Thread(task)
    thread.setName("par update thread")
    thread.setPriority(10)
    thread
  })

  case class ChunkEnts(chunk: Chunk, ents: Map[AnyEntID, AnyEnt]) extends GetPos {
    override def pos: V3I = chunk.pos
  }
  object ChunkEnts {
    def elevate(chunk: Chunk) = ChunkEnts(chunk, Map.empty)
  }
  case class Exchange[T, I](before: Option[T], after: Option[T])(implicit identity: T => I) {
    def andThen(other: Exchange[T, I]) = Exchange(before, other.after)(identity)
    def iden: Option[I] = this match {
      case Exchange(Some(t), _) => Some(identity(t))
      case Exchange(_, Some(t)) => Some(identity(t))
      case Exchange(None, None) => None
    }
  }
  object Exchange {
    def ends[T, I](exchanges: Seq[Exchange[T, I]]): Seq[Exchange[T, I]] = {
      val map = new mutable.HashMap[I, Exchange[T, I]]
      for (exchange <- exchanges) {
        exchange.iden.foreach(i => {
          if (map.contains(i))
            map(i) = map(i) andThen exchange
          else
            map(i) = exchange
        })
      }
      map.values.toSeq
    }
  }
  type ChunkExchange = Exchange[Chunk, V3I]
  object ChunkExchange {
    val nill = ChunkExchange(None, None)
    def apply(before: Option[Chunk], after: Option[Chunk]): ChunkExchange = Exchange(before, after)(_.pos)
    def unapply(exchange: ChunkExchange) = Some((exchange.before, exchange.after))
  }
  type EntExchange = Exchange[AnyEnt, AnyEntID]
  object EntExchange {
    val nill = EntExchange(None, None)
    def apply(before: Option[AnyEnt], after: Option[AnyEnt]): EntExchange = Exchange(before, after)(_.id)
    def unapply(exchange: EntExchange) = Some((exchange.before, exchange.after))
  }
  case class ChangeSummary(chunks: Seq[ChunkExchange], ents: Seq[EntExchange])
}

object SingleContinuum {
  case object IncompleteKey extends AsyncSave.DataKey[Seq[UpdateEffect]] {
    override def code: Long = 982734656598237465L
    override def default: Seq[UpdateEffect] = Seq.empty
  }
}

// holds a single world and manages impure mechanism
class SingleContinuum(save: AsyncSave[ChunkEnts]) {
  // these are volatile so that they can be read by other threads, like the rendering thread
  // we store them in one reference so it can be updated atomically, lock-free
  @volatile private var _timeAndCurr: (Long, SingleWorld) =
    (0, SingleWorld(Map.empty, Map.empty, V3ISet.empty, V3ISet.empty, Set.empty, Set.empty, BBox.empty))
  def timeAndCurr: (Long, SingleWorld) = _timeAndCurr
  def time: Long = _timeAndCurr._1
  def curr: SingleWorld = _timeAndCurr._2

  // these are useful for world evals and delayed effect integration
  private implicit val cfulfill = new FulfillmentContext[V3I, Chunk]
  private implicit val tfulfill = new FulfillmentContext[V3I, Terrain]
  private implicit val efulfill = new FulfillmentContext[AnyEntID, AnyEnt]

  // this is useful for pended events or asynchronous requests
  // they are combined with runnable hooks for managing the incomplete system
  val runNothing: Runnable = () => ()
  private val asyncEffects = new ConcurrentLinkedQueue[(Seq[UpdateEffect], Runnable)]

  // these track load operations, and are bound to IDs which are compared to values in a map
  // so that they can be invalidated
  private val cloadQueue = new ConcurrentLinkedQueue[(ChunkEnts, UUID)]
  private val cloadMap = new mutable.HashMap[V3I, UUID]
  private val tloadQueue = new ConcurrentLinkedQueue[(Terrain, UUID)]
  private val tloadMap = new mutable.HashMap[V3I, UUID]

  // these chunks will be put in the world when they are within chunk domain
  private var pendingChunkPuts = Seq.empty[PutChunk]

  // these things must be saved to the DB upon closing
  private val incomplete: FutChain[Map[UUID, UpdateEffect]] = new FutChain(Map.empty, _.run())

  // to initialize, load the incomplete seq from the save, and just perform a tick with them as externs
  update(V3ISet.empty, V3ISet.empty, save.getKey(IncompleteKey).await)

  // table of service procedures
  val serviceProcedures = new ServiceTagTable[ServiceProcedure]
  serviceProcedures += new PhysicsServiceProcedure

  for (procedure <- serviceProcedures.toSeq)
    procedure.begin()

  // update the world and return externally handled effects
  def update(cdomain: V3ISet, tdomain: V3ISet, externs: Seq[UpdateEffect]): Seq[UpdateEffect] = {
    // next world accumulator
    var next = curr

    // functions for handling chunk removals or replacements
    def dispose0(chunk: Chunk): Unit = {
      for (sp <- chunk.declareDisposable)
        sp.dispose()
    }
    def dispose1(replace: ChunkExchange): Unit = replace match {
      case ChunkExchange(Some(before), Some(after)) =>
        for (sp <- after.whatShouldDispose(before))
          sp.dispose()
      case ChunkExchange(Some(before), None) =>
        for (sp <- before.declareDisposable)
          sp.dispose()
      case _ =>
    }
    def dispose2(chunks: Seq[Chunk]): Unit =
      for (chunk <- chunks) dispose0(chunk)
    def dispose3(replaces: Seq[ChunkExchange]): Unit =
      for (replace <- replaces) dispose1(replace)

    // downgrade chunks that left the chunk domain
    // push them to the save
    // remove them from the load map
    // remove them from the cfulfill
    // dispose of removed chunks
    {
      val downgrade: Seq[V3I] = (next.cdomain -- cdomain).toSeq
      save.push(downgrade.flatMap(next.chunkEnts))
      val (n, c) = next downgrade downgrade
      next = n
      dispose2(c)
      cloadMap --= downgrade
      cfulfill.remove(downgrade)
    }

    // remove chunks/terrain that left the terrain domain
    // essentially same as previous
    {
      val remove: Seq[V3I] = (next.tdomain -- tdomain).toSeq
      save.push(remove.flatMap(next.chunkEnts))
      //next --= remove
      val (n, c) = next -- remove
      next = n
      dispose2(c)
      tloadMap --= remove
      tfulfill.remove(remove)
    }

    // now for chunks that have entered the domain, load them asynchronously
    {
      val (cfuts, tfuts) = save.pull((cdomain -- next.cdomain).toSeq, (tdomain -- next.tdomain).toSeq)
      for ((p, fut) <- cfuts) {
        val loadID = UUID.randomUUID()
        cloadMap += p -> loadID
        fut.map(chunk => cloadQueue.add((chunk, loadID)))
      }
      for ((p, fut) <- tfuts) {
        val loadID = UUID.randomUUID()
        tloadMap += p -> loadID
        fut.map(terr => tloadQueue.add((terr, loadID)))
      }
    }

    // now that we're done with domain transitioning, update to the new domain
    next = next.setDomain(cdomain, tdomain)

    // now for the second part of async loading - integrating chunks and terrain that have completed their
    // asynchronous loading process
    {
      // since there's no urgency to do this now, we put a time limit on time spent integrating
      val timeLimit = 10 milliseconds
      var timer = Timer.start
      // first we'll load terrain since chunks will override terrain
      // we're gonna add the chunks to an array buffer so we can integrate them all simultaneously
      // which reduces locking and other operations
      val tbuffer = new mutable.ArrayBuffer[Terrain]
      // we're actually using !isEmpty instead of size > 0 because sizeof operations for concurrent linked queue
      // involves locking and is linear time complexity
      while (timer.elapsed < timeLimit && !tloadQueue.isEmpty) {
        val (terr, loadID) = tloadQueue.remove()
        if (tloadMap.get(terr.pos).contains(loadID)) {
          tbuffer += terr
        }
      }
      // now we integrate them
      {
        val (n, c) = next putTerrains tbuffer
        next = n
        dispose3(c)
      }
      tloadMap --= tbuffer.map(_.pos)
      tfulfill.put(tbuffer.map(terr => (terr.pos, terr)))

      // reset the timer and essentially do the same thing with chunks
      // except we also put chunks in the tfulfill
      // and we also remove them from cloadMap
      timer = Timer.start
      val cbuffer = new mutable.ArrayBuffer[ChunkEnts]
      while (timer.elapsed < timeLimit && !cloadQueue.isEmpty) {
        val (chunkEnts, loadID) = cloadQueue.remove()
        if (cloadMap.get(chunkEnts.chunk.pos).contains(loadID)) {
          cbuffer += chunkEnts
        }
      }
      {
        val (n, c, e) = next putChunkEnts cbuffer
        next = n
        dispose3(c)
        e foreach {
          case EntExchange(_, Some(after)) => efulfill.put(after.id, after)
          case EntExchange(Some(before), None) => efulfill.remove(before.id)
          case EntExchange(None, None) => ()
        }
      }
      cloadMap --= cbuffer.map(_.chunk.pos)
      tloadMap --= cbuffer.map(_.chunk.pos)
      cfulfill.put(cbuffer.map(ce => (ce.chunk.pos, ce.chunk)))
      tfulfill.put(cbuffer.map(ce => (ce.chunk.pos, ce.chunk.terrain)))
    }

    // accumulate effects which we input to the update function
    // we start with externs
    val effectsIn = externs.toBuffer
    // and then all effects async effects that are ready and have been queued
    while (!asyncEffects.isEmpty) {
      val (e, r) = asyncEffects.remove()
      effectsIn ++= e
      r.run()
    }
    // and then all pending chunk puts that are now within domain
    ;{
      val (putNow, putLater) = pendingChunkPuts.partition(pc => cdomain contains pc.c.pos)
      effectsIn ++= putNow
      pendingChunkPuts = putLater
    }

    // now we let the world update itself purely, returning its updated version, and externally handled effects
    val (updated, effectsOut, changed) = next.parUpdate(time, effectsIn, serviceProcedures)
    next = updated

    // update the fulfillment contexts with the changes
    {
      val putc = new mutable.ArrayBuffer[Chunk]
      Exchange.ends(changed.chunks) foreach {
        case ChunkExchange(_, Some(after)) => putc += after
      }
      cfulfill.put(putc.map(c => (c.pos, c)))
      tfulfill.put(putc.map(c => (c.pos, c.terrain)))
    }
    {
      val pute = new mutable.ArrayBuffer[AnyEnt]
      val reme = new mutable.ArrayBuffer[AnyEntID]
      Exchange.ends(changed.ents) foreach {
        case EntExchange(_, Some(after)) => pute += after
        case EntExchange(Some(before), None) => reme += before.id
      }
      efulfill.put(pute.map(e => (e.id, e)))
      efulfill.remove(reme)
    }

    // perform all chunk change disposals
    dispose3(changed.chunks)

    // now let's group the outputted effects
    val grouped = new GroupedEffects(effectsOut)

    // handle the async request effects
    {
      val pack = WEval.EvalAsync(UniExecutor.getService, cfulfill, tfulfill, efulfill)
      for (make@MakeRequest(Request(eval, id), onComplete) <- grouped.bin(MakeRequest)) {
        val incompleteID = UUID.randomUUID()
        incomplete.update(_ + (incompleteID -> make))
        new AsyncEval(eval)().fut(WEval.input, pack).map(result => {
          val requested = new Requested(id, result)
          val effects = onComplete(requested)
          asyncEffects.add((effects, () => incomplete.update(_ - incompleteID)))
        })
      }
    }

    // handle pending put chunks (pended because not in domain)
    pendingChunkPuts ++= grouped.bin(PutChunk)

    // handle pending ent puts (pended because containing chunk doesn't exist
    for (pe@PutEnt(ent) <- grouped.bin(PutEnt)) {
      val incompleteID = UUID.randomUUID()
      incomplete.update(_ + (incompleteID -> pe))
      cfulfill.fut(ent.chunkPos).map(c => asyncEffects.add((Seq(pe), () => incomplete.update(_ - incompleteID))))
    }

    // handle pending ent rems (pended because ent not found)
    for (re@RemEnt(id) <- grouped.bin(RemEnt)) {
      val incompleteID = UUID.randomUUID()
      incomplete.update(_ + (incompleteID -> re))
      efulfill.fut(id).map(e => asyncEffects.add((Seq(re), () => incomplete.update(_ - incompleteID))))
    }

    // update continuum state
    _timeAndCurr = (time + 1, next)

    // return the types of effects handled externally
    grouped.bin(SoundEffect) ++ grouped.bin(LogEffect)
  }

  def close(): Promise = {
    // save the incomplete
    val incompleteSeq = incomplete.curr.await.values.to[Vector] ++ pendingChunkPuts
    val p1 = save.putKey(IncompleteKey, incompleteSeq)
    // close the save with all the chunks
    val p2 = save.close(curr.chunks.flatMap({
      case (p, Left(ce)) => Some(p -> ce)
      case _ => None
    }))
    // close the service procedures
    val p3 = Fut[Unit]({
      for (service <- serviceProcedures.toSeq)
        service.close()
    }, UniExecutor.execc)
    // merge the promises
    PromiseFold(Seq(p1, p2, p3))
  }
}