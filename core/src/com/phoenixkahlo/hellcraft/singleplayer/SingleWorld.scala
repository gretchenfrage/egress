package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentLinkedQueue, ThreadLocalRandom}

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
import com.phoenixkahlo.hellcraft.service.{Service, ServiceProcedure, ServiceTagTable, ServiceWorld}
import com.phoenixkahlo.hellcraft.singleplayer.AsyncSave.GetPos
import com.phoenixkahlo.hellcraft.singleplayer.SingleContinuum.IncompleteKey
import com.phoenixkahlo.hellcraft.singleplayer.SingleWorld.{apply => _, _}
import com.phoenixkahlo.hellcraft.util.collections.{BBox, ParGenMutHashMap, V3ISet}
import com.phoenixkahlo.hellcraft.util.threading._
import com.phoenixkahlo.hellcraft.util.time.Timer

import scala.concurrent.duration._
import scala.collection.{JavaConverters, mutable, parallel}
import scala.util.Random

/*
class ParUpdater(time: Long, gridIn: Map[V3I, Either[ChunkEnts, Terrain]], entsIn: AnyEntID => Option[V3I], exec: Runnable => Unit) {
  private val chunks = new ParGenMutHashMap[V3I, FutChain[Option[Chunk]]](p => new FutChain(gridIn.get(p).flatMap(_.left.toOption).map(_.chunk), exec))
  private val ents = new ParGenMutHashMap[AnyEntID, FutChain[Option[AnyEnt]]](id => new FutChain(entsIn(id).map(p => gridIn(p).left.get.ents(id)), exec))
  private val out = new ConcurrentLinkedQueue[UpdateEffect]

  def addOut(effect: UpdateEffect): Unit = out.add(effect)

  // function for evaluating update monads and accumulating state
  def eval[T](ue: UE[T]): Fut[(T, Seq[AcquiredState])] = ue match {
    case UEGen(fac) =>
      Fut((fac(), Seq.empty), exec)
    case ue: UEMap[_, T] =>
      def f[S](ue: UEMap[S, T]): Fut[(T, Seq[AcquiredState])] =
        eval(ue.src).map({ case (s, state) => (ue.func(s), state) })
      f(ue)
    case ue: UEFMap[_, T] =>
      def f[S](ue: UEFMap[S, T]): Fut[(T, Seq[AcquiredState])] =
        eval(ue.src).flatMap({
          case (s, state1) => eval(ue.func(s)).map({
            case (t, state2) => (t, state1 ++ state2)
          })
        })
      f(ue)
    case UEFilter(src: T, test: (T => Boolean)) =>
      eval(src).filter({ case (t, _) => test(t) }, exec)
    case UEChunk(p) =>
      val (chunkFut, settable) = chunks(p)().getAndSet(new SettableFut[Option[Chunk]])
      chunkFut.map(chunk => (chunk.asInstanceOf[T], Seq(AcquiredChunk(p, chunk, settable))), exec)
    case UETerrain(p) =>
      val (chunkFut, settable) = chunks(p)().getAndSet(new SettableFut[Option[Chunk]])
      chunkFut.map(chunk => ((chunk match {
        case Some(c) => Some(c.terrain)
        case None => gridIn.get(p).flatMap(_.right.toOption)
      }).asInstanceOf[T], Seq(AcquiredChunk(p, chunk, settable))), exec)
    case UEEnt(id) =>
      val (entFut, settable) = ents(id)().getAndSet(new SettableFut[Option[AnyEnt]])
      entFut.map(ent => (ent.asInstanceOf[T], Seq(AcquiredEnt(id, ent, settable))), exec)
    case UERand => Fut((new MRNG(ThreadLocalRandom.current.nextLong()).asInstanceOf[T], Seq.empty), exec)
    case UETime => Fut((time.asInstanceOf[T], Seq.empty), exec)
  }

  // handle the effect asynchronously, but retaining causal coherency
  def handle(effect: UpdateEffect): Unit = exec(() => {
    println(effect)
    effect match {
      // output only effects
      case effect: SoundEffect => addOut(effect)
      case effect: MakeRequest[_] => addOut(effect)
      case effect: LogEffect => addOut(effect)
      case effect: CallService[_, _] => addOut(effect)
      // simple asynchronous modification effects
      case PutChunk(chunk) => chunks(chunk.pos)().update(any => Some(chunk))
      case PutEnt(ent) => ents(ent.id)().update(any => Some(ent))
      case RemEnt(id) => ents(id)().update(any => None)
      // the event effect
      case Event(ue) =>
        // evaluate monad and process
        eval(ue).map({
          case (caused, acquired: Seq[AcquiredState]) =>
            // some effects we must handle synchronously for causal coherency
            // all settables must be set or the fut graph will freeze
            // some effects will be recursively handled asynchonously

            val out = new mutable.ArrayBuffer[UpdateEffect]

            var chunkResults: Map[V3I, Option[Chunk]] = acquired.flatMap({
              case AcquiredChunk(p, before, settable) => Some(p -> before)
              case _ => None
            }).toMap
            var entResults: Map[AnyEntID, Option[AnyEnt]] = acquired.flatMap({
              case AcquiredEnt(id, before, settable) => Some(id -> before): Option[(AnyEntID, Option[AnyEnt])]
              case _ => None
            }).toMap

            caused foreach {
              case PutChunk(chunk) if chunkResults.contains(chunk.pos) =>
                chunkResults += (chunk.pos -> Some(chunk))
              case PutEnt(ent) if entResults.contains(ent.id) =>
                entResults += (ent.id -> Some(ent))
              case RemEnt(id) if entResults.contains(id) =>
                entResults += (id -> None)
              case other => out += other
            }

            acquired.foreach {
              case AcquiredChunk(p, before, settable) => settable.set(chunkResults(p))
              case AcquiredEnt(id, before, settable) => settable.set(entResults(id))
            }

            out.foreach(handle)
        })
    }
  })

  def extract: (Map[V3I, Chunk], Map[AnyEntID, Option[AnyEnt]], Seq[UpdateEffect]) =
    (
      chunks.toSeq.flatMap({ case (p, chain) => chain.curr.query.get.map(chunk => (p -> chunk)) }).toMap,
      ents.toSeq.map({ case (id, chain) => (id -> chain.curr.query.get) }).toMap,
      {
        val jlist = new java.util.ArrayList[UpdateEffect]
        out.addAll(jlist)
        JavaConverters.asScalaBuffer(jlist)
      }
    )
}

object ParUpdater {
  sealed trait AcquiredState
  case class AcquiredChunk(p: V3I, before: Option[Chunk], fut: SettableFut[Option[Chunk]]) extends AcquiredState
  case class AcquiredEnt(id: AnyEntID, before: Option[AnyEnt], fut: SettableFut[Option[AnyEnt]]) extends AcquiredState
}
*/

case class SingleWorld(
                      chunks: Map[V3I, Either[ChunkEnts, Terrain]],
                      ents: Map[AnyEntID, V3I],
                      cdomain: V3ISet,
                      tdomain: V3ISet,
                      active: Set[V3I],
                      renderable: Set[V3I],
                      bbox: BBox
                      ) extends ClientWorld with ServiceWorld {
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
    /*
    val outEffects = new mutable.ArrayBuffer[UpdateEffect] // remember: reverse before returning
    val chunkChanges = new mutable.HashMap[V3I, Chunk]
    val entChanges = new mutable.HashMap[AnyEntID, Option[AnyEnt]]
    val chunkReplaces = new mutable.ArrayBuffer[ChunkExchange] // remember: reverse before returning
    */
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
          //chunkReplaces ++= r
          //chunkChanges += (chunk.pos -> chunk)
        } else outEffects += effect
      case effect@PutEnt(ent) =>
        if (world.chunk(ent.chunkPos).isDefined) {
          val (w, e) = world putEnt ent
          world = w
          entExhanges += e
          //world = world putEnt ent
          //entExhanges += EntExchange()
          //entChanges += (ent.id -> Some(ent))
        } else outEffects += effect
      case effect@RemEnt(id) =>
        if (world.findEntityUntyped(id).isDefined) {
          val (w, e) = world remEnt id
          world = w
          entExhanges += e
          //world = world remEnt id
          //entChanges += (id -> None)
        } else outEffects += effect
      case effect: CallService[_, _] =>
        def f[S <: Service, T](call: CallService[S, T]): Seq[UpdateEffect] =
          call.onComplete(PartialSyncEval(exec => services(call.service).apply(world, call.call)(exec)))
        f(effect).reverse.foreach(queue.addFirst)
      case effect@Event(ue) =>
        world.eval(ue, _time).reverse.foreach(queue.addFirst)
      case effect: IdenEvent => ???
    }

    (world, outEffects, ChangeSummary(chunkExchanges, entExhanges))
  }

  /*
  def parUpdate(time: Long, externs: Seq[UpdateEffect], services: ServiceTagTable[ServiceProcedure]): (SingleWorld, Seq[UpdateEffect], ChangeSummary) = {
    var world = this

    var outEffects = new mutable.ArrayBuffer[UpdateEffect]
    //var chunkChanges = new mutable.HashMap[V3I, Chunk]
    var entChanges = new mutable.HashMap[AnyEntID, Option[AnyEnt]]
    var chunkReplaces = new mutable.ArrayBuffer[ReplacedChunk]

    var in = externs ++ active.toSeq.flatMap(chunks(_).left.get.ents.values).flatMap(_.update)
    while (in.nonEmpty) {
      val exec: java.util.Queue[Runnable] = new java.util.LinkedList[Runnable]

      /*
      val count = new AtomicInteger(0)
      val complete = new SettableFut[Unit]
      def exec(task: Runnable): Unit = {
        count.incrementAndGet()
        UniExecutor.exec(() => {
          task.run()
          val curr = count.decrementAndGet()
          if (curr == 0)
            complete.set(())
        })
      }
      */


      val par = new ParUpdater(time, world.chunks, world.ents.get, exec.add)
      in.foreach(par.handle)

      //complete.await

      while (!exec.isEmpty)
        exec.remove().run()
      val (newChunks, newEnts, newEffects) = par.extract
      ;{
        val (n: SingleWorld, r: Seq[ReplacedChunk]) = world putChunks newChunks.values.toSeq
        world = n
        chunkReplaces ++= r
      }
      ;{
        entChanges ++= newEnts
        val put: Seq[AnyEnt] = newEnts.values.toSeq.flatten
        world = put.foldLeft(world)({ case (w, ent) => w putEnt ent })
        val rem: Seq[AnyEntID] = newEnts.toSeq.filter(_._2.isEmpty).map(_._1)
        world = rem.foldLeft(world)({ case (w, id) => w remEnt id })
      }
      val grouped = new GroupedEffects(newEffects)
      ;{
        def f[S <: Service, T](call: CallService[S, T]): Seq[UpdateEffect] =
          call.onComplete(PartialSyncEval(exec => services(call.service).apply(world, call.call)(exec)))
        in = grouped.bin(CallService).flatMap(f(_))
      }
      outEffects ++= grouped.bin(SoundEffect)
      outEffects ++= grouped.bin(MakeRequest)
      outEffects ++= grouped.bin(LogEffect)
    }

    val chunkChanges = chunkReplaces.flatMap({
      case ReplacedChunk(old, Some(neu)) => Some(old.pos -> neu)
      case _ => None
    }).toMap

    (world, outEffects, ChangeSummary(chunkChanges, entChanges.toMap, chunkReplaces))
  }
  */
}
object SingleWorld {
  case class ChunkEnts(chunk: Chunk, ents: Map[AnyEntID, AnyEnt]) extends GetPos {
    override def pos: V3I = chunk.pos
  }
  object ChunkEnts {
    def elevate(chunk: Chunk) = ChunkEnts(chunk, Map.empty)
  }
  //case class ChangeSummary(chunks: Map[V3I, Chunk], ents: Map[AnyEntID, Option[AnyEnt]], replaced: Seq[ReplacedChunk])
  //case class ReplacedChunk(old: Chunk, repl: Option[Chunk])
  case class ChunkExchange(before: Option[Chunk], after: Option[Chunk]) {
    def andThen(other: ChunkExchange) = ChunkExchange(before, other.after)
  }
  case class EntExchange(before: Option[AnyEnt], after: Option[AnyEnt]) {
    def andThen(other: EntExchange) = EntExchange(before, other.after)
  }
  object EntExchange {
    val nill = EntExchange(None, None)
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
      //next = next putChunkEnts cbuffer
      {
        val (n, c) = next putChunkEnts cbuffer
        next = n
        dispose3(c)
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
      //effectsIn += asyncEffects.remove()
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
    val (updated, effectsOut, changed) = next.update(time, effectsIn, serviceProcedures)
    next = updated

    // update the fulfillment contexts with the changes

    cfulfill.put(changed.chunks.toSeq)
    tfulfill.put(changed.chunks.values.toSeq.map(chunk => (chunk.pos, chunk.terrain)))
    ;{
      val addedEnts = new mutable.ArrayBuffer[(AnyEntID, AnyEnt)]
      val remedEnts = new mutable.ArrayBuffer[AnyEntID]
      changed.ents foreach {
        case (id, Some(ent)) => addedEnts += ((id, ent))
        case (id, None) => remedEnts += id
      }
      efulfill.put(addedEnts)
      efulfill.remove(remedEnts)
    }


    // process the chunk replacements from the update function
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