package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID
import java.util.concurrent.{ConcurrentLinkedQueue, ThreadLocalRandom}

import com.phoenixkahlo.hellcraft.core.client.{ClientRenderWorld, ClientWorld}
import com.phoenixkahlo.hellcraft.core.entity.{AnyEnt, AnyEntID, EntID, Entity}
import com.phoenixkahlo.hellcraft.core.eval.{AsyncEval, WEval}
import com.phoenixkahlo.hellcraft.core.event.UE.UEGen
import com.phoenixkahlo.hellcraft.core.event.UE.UEMap
import com.phoenixkahlo.hellcraft.core.event.UE.UEFMap
import com.phoenixkahlo.hellcraft.core.event.UE.UEFilter
import com.phoenixkahlo.hellcraft.core.event.UE.UEChunk
import com.phoenixkahlo.hellcraft.core.event.UE.UETerrain
import com.phoenixkahlo.hellcraft.core.event.UE.UEEnt
import com.phoenixkahlo.hellcraft.core.event.UE
import com.phoenixkahlo.hellcraft.core.request.{Request, Requested}
import com.phoenixkahlo.hellcraft.core.util.GroupedEffects
import com.phoenixkahlo.hellcraft.core.{Chunk, Context, Event, EventID, IContext, LogEffect, MakeRequest, PutChunk, PutEnt, RemEnt, SoundEffect, Terrain, UpdateEffect, event}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.collections.V3ISet
import com.phoenixkahlo.hellcraft.util.threading.{FulfillmentContext, UniExecutor}
import com.phoenixkahlo.hellcraft.util.time.Timer

import scala.concurrent.duration._
import scala.collection.mutable
import scala.util.Random

private case class ChunkEnts(chunk: Chunk, ents: Map[AnyEntID, AnyEnt])
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
  override def chunk(p: V3I) =
    chunks.get(p) match {
      case Some(Left(ChunkEnts(chunk, _))) => Some(chunk)
      case _ => None
    }

  // terrain lookup
  override def terrain(p: V3I) =
    chunks.get(p).map({
      case Left(ChunkEnts(chunk, _)) => chunk.terrain
      case Right(terrain) => terrain
    })

  // entity lookup
  override def findEntity[E <: Entity[E]](id: EntID[E]) =
    ents.get(id).map(p => chunks(p).left.get.ents.get(id).asInstanceOf[E])

  // bounding box
  override def bounds = bbox()

  // loaded chunks
  override def loadedChunks =
    chunks.keySet.toSeq.filter(chunks(_).isLeft)

  // loaded terrains
  override def loadedTerrain =
    chunks.keySet.toSeq

  // extend world to renderable version
  def renderable(_ftime: Float, _interp: Float): ClientRenderWorld =
    new SingleWorld(chunks, ents, cdomain, tdomain, active, renderable, bbox) with ClientRenderWorld {
      override def renderableChunks = renderable.toSeq.map(chunks).map(_.left.get.chunk)

      override def ftime = _ftime

      override def interp = _interp
    }

  // remove chunks/terrains
  def --(ps: Seq[V3I]): SingleWorld = {
    val es = ps.flatMap(chunks.get).flatMap(_.left.toOption).flatMap(_.ents.keySet)
    SingleWorld(chunks -- ps, ents -- es, cdomain, tdomain, active -- ps, renderable -- ps, bbox -- ps)
  }

  // remove chunk/terrain
  def -(p: V3I): SingleWorld = {
    val es = chunks.get(p).flatMap(_.left.toOption).flatMap(_.ents.keySet)
    SingleWorld(chunks - p, ents -- es, cdomain, tdomain, active - p, renderable - p, bbox - p)
  }

  // downgrade chunks to terrains
  def downgrade(ps: Seq[V3I]): SingleWorld = {
    val es = ps.flatMap(chunks.get).flatMap(_.left.toOption).flatMap(_.ents.keySet.toSeq)
    copy(chunks = ps.foldLeft(chunks)({
      case (map, p) => map.get(p) match {
        case Some(Left(ChunkEnts(chunk, _))) => map.updated(p, Right(chunk.terrain))
        case _ => map
      }
    }), ents = ents -- es, active = active -- ps, renderable = renderable -- ps, bbox = bbox -- ps)
  }

  // put chunks in world
  def putChunks(cs: Seq[Chunk]): SingleWorld = {
    if (cs.isEmpty) this
    else copy(
      chunks = cs.foldLeft(chunks)({
        case (map, chunk) => map.get(chunk.pos) match {
          case Some(Left(ChunkEnts(c, e))) => map.updated(chunk.pos, Left(ChunkEnts(chunk, e)))
          case _ => map.updated(chunk.pos, Left(ChunkEnts(chunk, Map.empty)))
        }
      }),
      renderable = renderable ++ cs.filter(_.isRenderable).map(_.pos),
      bbox = bbox ++ cs.map(_.pos)
    )
  }

  // put terrains in world
  def putTerrains(ts: Seq[Terrain]): SingleWorld = {
    if (ts.isEmpty) this
    else copy(
      chunks = ts.foldLeft(chunks)({
        case (map, terrain) =>
          if (map.get(terrain.pos).exists(_.isLeft))
            println("warn: put terrains is overriding chunk")
          map.updated(terrain.pos, Right(terrain))
      }),
      renderable = renderable -- ts.map(_.pos),
      bbox = bbox -- ts.map(_.pos)
    )
  }

  // put entity in world
  def putEnt(ent: AnyEnt): SingleWorld = {
    chunks.get(ent.chunkPos) match {
      case Some(Left(ChunkEnts(chunk, ents))) =>
        copy(
          chunks = chunks.updated(ent.chunkPos, Left(ChunkEnts(chunk, ents + (ent.id -> ent)))),
          active = active + ent.chunkPos,
          renderable = renderable + ent.chunkPos
        )
      case _ =>
        println("warn: failed to put ent, chunk non-existent")
        this
    }
  }

  // remove entity from world
  def remEnt(id: AnyEntID): SingleWorld = {
    ents.get(id).map(p => chunks.get(p) match {
      case Some(Left(ChunkEnts(chunk, ents))) =>
        if (ents.contains(id)) {
          copy(
            chunks = chunks.updated(p, Left(ChunkEnts(chunk, ents - id))),
            active = if (ents.size == 1) active - p else active,
            renderable = if (ents.size == 1 && !chunk.isRenderable) renderable - p else renderable
          )
        } else {
          println("warn: failed to remove ent, not present in chunk")
          this
        }
      case _ =>
        println("warn: failed to remove ent, chunk non-existent")
        this
    }).getOrElse({
      println("warn: failed to remove ent, not found")
      this
    })
  }

  // update the domain
  def setDomain(_cdomain: V3ISet, _tdomain: V3ISet): SingleWorld =
    copy(cdomain = _cdomain, tdomain = _tdomain)

  // evaluate a UE monad
  def eval[T](ue: UE[T]): T = ue match {
    case ue@UEGen(fac) => fac()
    case ue@UEMap(src, func) =>
      def f[S](ue: UEMap[S, T]): T = ue.func(eval(ue.src))
      f(ue)
    case ue@UEFMap(src, func) =>
      def f[S](ue: UEFMap[S, T]): T = eval(ue.func(eval(ue.src)))
      f(ue)
    case ue@UEFilter(src, test) =>
      val t = eval(src)
      if (!test(t))
        println("warn: UE filter monad failed filter (ignoring)")
      t
    case ue@UEChunk(p) => chunk(p).asInstanceOf[T]
    case ue@UETerrain(p) => terrain(p).asInstanceOf[T]
    case ue@UEEnt(id) => findEntity(id).asInstanceOf[T]
  }

  // update to a new world, and get output effects
  def update(_time: Long, externs: Seq[UpdateEffect]): (SingleWorld, Seq[UpdateEffect]) = {
    // recursive function for computing a phase
    def phase(n: Byte, world: SingleWorld, in: Seq[UpdateEffect]): (SingleWorld, Seq[UpdateEffect]) = {
      // initialize the context
      Context.init(new IContext {
        val random = new Random
        override def time: Long = _time
        override def randInt(): Int = random.nextInt()
        override def randDouble(): Double = random.nextDouble()
        override def randFloat(): Float = random.nextFloat()
        override def eventID(): EventID = EventID(_time, n, UUID.randomUUID())
      })
      // group the effects by type
      val grouped = new GroupedEffects(in)
      // get the effects we will output, that aren't integrated purely
      val out = new mutable.ArrayBuffer[UpdateEffect]
      out ++= grouped.bin(SoundEffect)
      out ++= grouped.bin(MakeRequest)
      out ++= grouped.bin(LogEffect)
      // apply all the updaters to the world
      // if they cannot be applied now, they are added to the out buffer
      var next: SingleWorld = world
      ;{
        val (putChunksNow, putChunksLater) = grouped.bin(PutChunk).partition(pc => cdomain contains pc.c.pos)
        next = next putChunks putChunksNow.map(_.c)
        out ++= putChunksLater
      }
      {
        val (putEntsNow, putEntsLater) = grouped.bin(PutEnt).partition(pe => chunk(pe.ent.chunkPos).isDefined)
        next = putEntsNow.foldLeft(next)({ case (w, pe) => w putEnt pe.ent })
        out ++= putEntsLater
      }
      {
        val (remEntsNow, remEntsLater) = grouped.bin(RemEnt).partition(re => findEntity(re.id).isDefined)
        next = remEntsNow.foldLeft(next)({ case (w, re) => w remEnt re.id })
        out ++= remEntsLater
      }
      // evaluate the events
      val caused = grouped.bin(Event).flatMap(event => eval(event.eval))
      // close the context
      Context.end()
      // recurse if any further events are caused
      if (caused isEmpty)
        (next, out)
      else {
        val (w, e) = phase((n + 1).toByte, next, caused)
        (w, e ++ out)
      }
    }
    // get the entities' update effects
    def entEvents = active.toSeq.flatMap(chunks(_).left.get.ents.values).flatMap(_.update)
    // call the recursive function and return the result
    phase(0, this, entEvents ++ externs)
  }

}

// holds a single world and manages impure mechanism
class SingleContinuum(save: AsyncSave) {
  // these are volatile so that they can be read by other threads, like the rendering thread
  // we store them in one reference so it can be updated atomically, lock-free
  @volatile private var timeAndCurr: (Long, SingleWorld) =
    (0, SingleWorld(Map.empty, Map.empty, V3ISet.empty, V3ISet.empty, Set.empty, Set.empty, BBox.empty))
  def time: Long = timeAndCurr._1
  def curr: SingleWorld = timeAndCurr._2

  // these are useful for world evals and delayed event integration
  private implicit val cfulfill = new FulfillmentContext[V3I, Chunk]
  private implicit val tfulfill = new FulfillmentContext[V3I, Terrain]

  // let's just make this implicit
  private implicit val execService = UniExecutor.getService

  // this is useful for pended events or asynchronous requests
  private val asyncEffects = new ConcurrentLinkedQueue[UpdateEffect]

  // these track load operations, and are bound to IDs which are compared to values in a map
  // so that they can be invalidated
  private val cloadQueue = new ConcurrentLinkedQueue[(Chunk, UUID)]
  private val cloadMap = new mutable.HashMap[V3I, UUID]
  private val tloadQueue = new ConcurrentLinkedQueue[(Terrain, UUID)]
  private val tloadMap = new mutable.HashMap[V3I, UUID]

  // update the world and return externally handled effects
  def update(cdomain: V3ISet, tdomain: V3ISet, externs: Seq[UpdateEffect]): Seq[UpdateEffect] = {
    // next world accumulator
    var next = curr

    // downgrade chunks that left the chunk domain
    // push them to the save
    // remove them from the load map
    // remove them from the cfulfill
    {
      val downgrade: Seq[V3I] = (next.cdomain -- cdomain).toSeq
      save.push(downgrade.flatMap(next.chunk))
      next = next downgrade downgrade
      cloadMap --= downgrade
      cfulfill.remove(downgrade)
    }

    // remove chunks/terrain that left the terrain domain
    // essentially same as previous
    {
      val remove: Seq[V3I] = (next.tdomain -- tdomain).toSeq
      save.push(remove.flatMap(next.chunk))
      next --= remove
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
      next = next putTerrains tbuffer
      tloadMap --= tbuffer.map(_.pos)
      tfulfill.put(tbuffer.map(terr => (terr.pos, terr)))

      // reset the timer and essentially do the same thing with chunks
      // except we also put chunks in the tfulfill
      timer = Timer.start
      val cbuffer = new mutable.ArrayBuffer[Chunk]
      while (timer.elapsed < timeLimit && !cloadQueue.isEmpty) {
        val (chunk, loadID) = cloadQueue.remove()
        if (cloadMap.get(chunk.pos).contains(loadID)) {
          cbuffer += chunk
        }
      }
      next = next putChunks cbuffer
      cloadMap --= cbuffer.map(_.pos)
      cfulfill.put(cbuffer.map(chunk => (chunk.pos, chunk)))
      tfulfill.put(cbuffer.map(chunk => (chunk.pos, chunk.terrain)))
    }

    // accumulate effects which we input to the update function
    // this is the extern effects, inputted to this function, plus all the effects in the async effects queue
    // we sort the async events by their IDs to prevent out-of-order integration
    val effectsIn: Seq[UpdateEffect] = {
      val async = new mutable.ArrayBuffer[UpdateEffect]
      while (!asyncEffects.isEmpty)
        async += asyncEffects.remove()
      val (isEvent, notEvent) = async.partition(_.effectType == Event)
      externs ++ notEvent ++ isEvent.map(_.asInstanceOf[Event]).sortBy(_.id)
    }

    // now we let the world update itself purely, returning its updated version, and externally handled effects
    val (updated, effectsOut) = next.update(time, effectsIn)
    next = updated

    // now let's group the outputted effects
    val grouped = new GroupedEffects(effectsOut)

    // handle the async request effects
    {
      val pack = WEval.EvalAsync(execService, cfulfill, tfulfill)
      for (MakeRequest(Request(eval, id), onComplete) <- grouped.bin(MakeRequest)) {
        new AsyncEval(eval)().fut(WEval.input, pack).map(result => {
          val requested = new Requested(id, result)
          // TODO: this requires a context activation
          val effects = onComplete(requested)
          effects.foreach(asyncEffects.add)
        })
      }
    }

    // TODO: handle pending put chunks, put ents, and rem ents

    // update continuum state
    timeAndCurr = (time + 1, next)

    // return the types of effects handled externally
    grouped.bin(SoundEffect) ++ grouped.bin(LogEffect)
  }
}