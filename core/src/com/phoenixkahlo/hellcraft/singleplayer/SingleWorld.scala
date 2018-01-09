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
import com.phoenixkahlo.hellcraft.core.event.{UE, UEContext, UEContextImpl}
import com.phoenixkahlo.hellcraft.core.request.{Request, Requested}
import com.phoenixkahlo.hellcraft.core.util.GroupedEffects
import com.phoenixkahlo.hellcraft.core.{Chunk, Event, EventID, LogEffect, MakeRequest, PutChunk, PutEnt, RemEnt, SoundEffect, Terrain, UpdateEffect, event}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.singleplayer.SingleWorld.{ChangeSummary, ChunkEnts}
import com.phoenixkahlo.hellcraft.util.collections.{BBox, V3ISet}
import com.phoenixkahlo.hellcraft.util.threading.{FulfillmentContext, Promise, UniExecutor}
import com.phoenixkahlo.hellcraft.util.time.Timer

import scala.concurrent.duration._
import scala.collection.mutable
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
  override def chunk(p: V3I) =
    chunks.get(p) match {
      case Some(Left(ChunkEnts(chunk, _))) => Some(chunk)
      case _ => None
    }

  // chunk and ents lookup
  def chunkEnts(p: V3I): Option[ChunkEnts] = chunks.get(p).flatMap(_.left.toOption)

  // terrain lookup
  override def terrain(p: V3I) =
    chunks.get(p).map({
      case Left(ChunkEnts(chunk, _)) => chunk.terrain
      case Right(terrain) => terrain
    })

  // entity lookup
  override def findEntity[E <: Entity[E]](id: EntID[E]) =
    ents.get(id).flatMap(p => chunks(p).left.get.ents.get(id).asInstanceOf[Option[E]])

  // untyped entity lookup
  def findEntityUntyped(id: AnyEntID): Option[AnyEnt] =
    ents.get(id).flatMap(p => chunks(p).left.get.ents.get(id))

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

      override def renderableEnts = active.toSeq.map(chunks).flatMap(_.left.get.ents.values)

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
    val es = chunks.get(p).flatMap(_.left.toOption).toSeq.flatMap(_.ents.keySet)
    SingleWorld(chunks - p, ents -- es, cdomain, tdomain, active - p, renderable - p, bbox - p)
  }

  // downgrade chunks to terrains
  def downgrade(ps: Seq[V3I]): SingleWorld = {
    val es = ps.flatMap(chunks.get).flatMap(_.left.toOption).flatMap(_.ents.keySet)
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

  // put entity in world without removing it first
  private def _putEnt(ent: AnyEnt): SingleWorld = {
    chunks.get(ent.chunkPos) match {
      case Some(Left(ChunkEnts(chunk, entMap))) =>
        copy(
          chunks = chunks.updated(ent.chunkPos, Left(ChunkEnts(chunk, entMap + (ent.id -> ent)))),
          ents = ents + (ent.id -> ent.chunkPos),
          active = active + ent.chunkPos,
          renderable = renderable + ent.chunkPos
        )
      case _ =>
        println("warn: failed to put ent, chunk non-existent")
        this
    }
  }

  // update the entity by removing then putting
  def putEnt(ent: AnyEnt): SingleWorld = remEnt(ent.id)._putEnt(ent)

  // put a sequence of chunk/ents
  def putChunkEnts(chunkEnts: Seq[ChunkEnts]): SingleWorld = {
    var w = this putChunks chunkEnts.map(_.chunk)
    for {
      ce <- chunkEnts
      ent <- ce.ents.values
    } w = w putEnt ent
    w
  }

  // remove entity from world
  def remEnt(id: AnyEntID): SingleWorld = {
    ents.get(id).map(p => chunks.get(p) match {
      case Some(Left(ChunkEnts(chunk, ents))) =>
        if (ents.contains(id)) {
          copy(
            chunks = chunks.updated(p, Left(ChunkEnts(chunk, ents - id))),
            active = if (ents.size == 1) active - p else active,
            renderable = if (!chunk.isRenderable) renderable - p else renderable
          )
        } else {
          println("warn: failed to remove ent, not present in chunk")
          this
        }
      case _ =>
        println("warn: failed to remove ent, chunk non-existent")
        this
    }).getOrElse({
      //println("warn: failed to remove ent, not found")
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
      def f[S, T](ue: UEMap[S, T]): T = ue.func(eval(ue.src))
      f(ue)
    case ue@UEFMap(src, func) =>
      def f[S, T](ue: UEFMap[S, T]): T = eval(ue.func(eval(ue.src)))
      f(ue)
    case ue@UEFilter(src: UE[T], test: (T => Boolean)) =>
      val t: T = eval(src)
      if (!test(t))
        println("warn: UE filter monad failed filter (ignoring)")
      t
    case ue@UEChunk(p) => chunk(p).asInstanceOf[T]
    case ue@UETerrain(p) => terrain(p).asInstanceOf[T]
    case ue@UEEnt(id) => findEntityUntyped(id).asInstanceOf[T]
  }

  // update to a new world, and get output effects
  def update(_time: Long, externs: Seq[UpdateEffect]): (SingleWorld, Seq[UpdateEffect], ChangeSummary) = {
    // recursive function for computing a phase
    def phase(n: Byte, world: SingleWorld, in: Seq[UpdateEffect], changed: ChangeSummary):
        (SingleWorld, Seq[UpdateEffect], ChangeSummary) = {
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
      var chunkChanges = changed.chunks
      var entChanges = changed.ents
      ;{
        val (putChunksNow, putChunksLater) = grouped.bin(PutChunk).partition(pc => cdomain contains pc.c.pos)
        next = next putChunks putChunksNow.map(_.c)
        chunkChanges ++= putChunksNow.map(pc => (pc.c.pos, pc.c))
        out ++= putChunksLater
      }
      {
        val (putEntsNow, putEntsLater) = grouped.bin(PutEnt).partition(pe => chunk(pe.ent.chunkPos).isDefined)
        next = putEntsNow.foldLeft(next)({ case (w, pe) => w putEnt pe.ent })
        entChanges ++= putEntsNow.map(pe => (pe.ent.id, Some(pe.ent)))
        out ++= putEntsLater
      }
      {
        val (remEntsNow, remEntsLater) = grouped.bin(RemEnt).partition(re => findEntityUntyped(re.id).isDefined)
        next = remEntsNow.foldLeft(next)({ case (w, re) => w remEnt re.id })
        entChanges ++= remEntsNow.map(re => (re.id, None))
        out ++= remEntsLater
      }
      // initialize the context
      UEContext.init(new UEContextImpl {
        val random = new Random
        override def time: Long = _time
        override def randInt(): Int = random.nextInt()
        override def randDouble(): Double = random.nextDouble()
        override def randFloat(): Float = random.nextFloat()
      })
      // evaluate the events
      val caused = grouped.bin(Event).flatMap(event => eval(event.eval))
      // close the context
      UEContext.end()
      // recurse if any further events are caused
      if (caused isEmpty)
        (next, out, ChangeSummary(chunkChanges, entChanges))
      else {
        val (w, e, c) = phase((n + 1).toByte, next, caused, ChangeSummary(chunkChanges, entChanges))
        (w, e ++ out, c)
      }
    }
    // get the entities' update effects
    def entEvents = active.toSeq.flatMap(chunks(_).left.get.ents.values).flatMap(_.update)
    // call the recursive function and return the result
    phase(0, this, entEvents ++ externs, ChangeSummary(Map.empty, Map.empty))
  }

}
object SingleWorld {
  case class ChunkEnts(chunk: Chunk, ents: Map[AnyEntID, AnyEnt]) extends GetPos {
    override def pos: V3I = chunk.pos
  }
  object ChunkEnts {
    def elevate(chunk: Chunk) = ChunkEnts(chunk, Map.empty)
  }
  case class ChangeSummary(chunks: Map[V3I, Chunk], ents: Map[AnyEntID, Option[AnyEnt]])
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
  private val asyncEffects = new ConcurrentLinkedQueue[UpdateEffect]

  // these track load operations, and are bound to IDs which are compared to values in a map
  // so that they can be invalidated
  private val cloadQueue = new ConcurrentLinkedQueue[(ChunkEnts, UUID)]
  private val cloadMap = new mutable.HashMap[V3I, UUID]
  private val tloadQueue = new ConcurrentLinkedQueue[(Terrain, UUID)]
  private val tloadMap = new mutable.HashMap[V3I, UUID]

  // these chunks will be put in the world when they are within chunk domain
  private var pendingChunkPuts = Seq.empty[PutChunk]

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
      save.push(downgrade.flatMap(next.chunkEnts))
      next = next downgrade downgrade
      cloadMap --= downgrade
      cfulfill.remove(downgrade)
    }

    // remove chunks/terrain that left the terrain domain
    // essentially same as previous
    {
      val remove: Seq[V3I] = (next.tdomain -- tdomain).toSeq
      save.push(remove.flatMap(next.chunkEnts))
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
      // and we also remove them from cloadMap
      timer = Timer.start
      val cbuffer = new mutable.ArrayBuffer[ChunkEnts]
      while (timer.elapsed < timeLimit && !cloadQueue.isEmpty) {
        val (chunkEnts, loadID) = cloadQueue.remove()
        if (cloadMap.get(chunkEnts.chunk.pos).contains(loadID)) {
          cbuffer += chunkEnts
        }
      }
      next = next putChunkEnts cbuffer
      cloadMap --= cbuffer.map(_.chunk.pos)
      tloadMap --= cbuffer.map(_.chunk.pos)
      cfulfill.put(cbuffer.map(ce => (ce.chunk.pos, ce.chunk)))
      tfulfill.put(cbuffer.map(ce => (ce.chunk.pos, ce.chunk.terrain)))
    }

    // accumulate effects which we input to the update function
    // we start with externs
    val effectsIn = externs.toBuffer
    // and then all effects async effects that are ready and have been queued
    while (!asyncEffects.isEmpty)
      effectsIn += asyncEffects.remove()
    // and then all pending chunk puts that are now within domain
    ;{
      val (putNow, putLater) = pendingChunkPuts.partition(pc => cdomain contains pc.c.pos)
      effectsIn ++= putNow
      pendingChunkPuts = putLater
    }


    // now we let the world update itself purely, returning its updated version, and externally handled effects
    val (updated, effectsOut, changed) = next.update(time, effectsIn)
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

    // now let's group the outputted effects
    val grouped = new GroupedEffects(effectsOut)

    // handle the async request effects
    {
      val pack = WEval.EvalAsync(UniExecutor.getService, cfulfill, tfulfill, efulfill)
      for (MakeRequest(Request(eval, id), onComplete) <- grouped.bin(MakeRequest)) {
        new AsyncEval(eval)().fut(WEval.input, pack).map(result => {
          val requested = new Requested(id, result)
          val effects = onComplete(requested)
          effects.foreach(asyncEffects.add)
        })
      }
    }

    // handle pending put chunks (pended because not in domain)
    pendingChunkPuts ++= grouped.bin(PutChunk)

    // handle pending ent puts (pended because containing chunk doesn't exist
    for (pe@PutEnt(ent) <- grouped.bin(PutEnt)) {
      cfulfill.fut(ent.chunkPos).map(c => asyncEffects.add(pe))
    }

    // handle pending ent rems (pended because ent not found)
    for (re@RemEnt(id) <- grouped.bin(RemEnt)) {
      efulfill.fut(id).map(e => asyncEffects.add(re))
    }

    // update continuum state
    _timeAndCurr = (time + 1, next)

    // return the types of effects handled externally
    grouped.bin(SoundEffect) ++ grouped.bin(LogEffect)
  }

  def close(): Promise = {
    save.close(curr.chunks.flatMap({
      case (p, Left(ce)) => Some(p -> ce)
      case _ => None
    }))
  }
}