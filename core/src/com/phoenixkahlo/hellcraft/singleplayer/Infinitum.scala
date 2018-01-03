package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID
import java.util.concurrent.{ConcurrentLinkedQueue, ThreadLocalRandom}

import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.core.eval.{AsyncEval, WEval}
import com.phoenixkahlo.hellcraft.core.request.{Request, Requested}
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.LeftOption
import com.phoenixkahlo.hellcraft.util.collections.{MergeBinned, TypeMatchingMap, V3ISet}
import com.phoenixkahlo.hellcraft.util.debugging.Profiler
import com.phoenixkahlo.hellcraft.util.threading._
import com.phoenixkahlo.hellcraft.util.time.Timer

import scala.concurrent.duration._
import scala.annotation.tailrec
import scala.collection.{SortedMap, mutable}

/**
  * A purely functional implementation of world, that stores a map of chunks, and three set of chunk indices,
  * which categorize which chunks are in which terrain state.
  */
class SWorld(
              override val time: Long,
              override val res: Int,
              val chunks: Map[V3I, Either[Chunk, Terrain]],
              val chunkDomain: V3ISet,
              val terrainDomain: V3ISet,
              val active: Set[V3I],
              val renderable: Set[V3I],
              val bbox: BBox
            ) extends World {

  override def chunkAt(p: V3I): Option[Chunk] =
    chunks.get(p).flatMap(LeftOption(_))

  override def terrainAt(p: V3I): Option[Terrain] =
    chunks.get(p).map({
      case Left(chunk) => chunk.terrain
      case Right(terrain) => terrain
    })

  override def debugLoadedChunks: Iterable[V3I] = chunks.keySet.toSeq.filter(chunks(_).isLeft)

  override def debugLoadedTerrain: Iterable[V3I] = chunks.keySet

  def renderable(_ftime: Float, _interp: Float): RenderWorld =
    new SWorld(time, res, chunks, chunkDomain, terrainDomain, active, renderable, bbox) with RenderWorld {
      override def renderableChunks = renderable.toSeq.map(chunks).map(_.left.get)

      override def ftime = _ftime

      override def interp: Float = _interp
    }


  override def findEntity(id: EntityID): Option[Entity] =
    chunks.values.toStream.flatMap(LeftOption(_)).flatMap(_.entities.get(id)).headOption

  override def boundingBox: (V3I, V3I) = bbox()

  /**
    * Remove those chunks from this world,
    */
  def --(ps: Seq[V3I]): SWorld = {
    new SWorld(time, res, chunks -- ps, chunkDomain, terrainDomain, active -- ps, renderable -- ps, bbox -- ps)
  }

  def downgrade(ps: Seq[V3I]): SWorld = {
    new SWorld(time, res, ps.foldLeft(chunks)({ case (map, p) => map.get(p) match {
      case Some(Left(chunk)) => map.updated(p, Right(chunk.terrain))
      case _ => map
    }}), chunkDomain, terrainDomain, active -- ps, renderable -- ps, bbox -- ps)
  }

  def -(p: V3I): SWorld = {
    new SWorld(time, res, chunks - p, chunkDomain, terrainDomain, active - p, renderable - p, bbox - p)
  }

  /**
    * Add those chunks to the world.
    */
  def ++(cs: Seq[Either[Chunk, Terrain]]): SWorld = {
    def pos(c: Either[Chunk, Terrain]): V3I = c match {
      case Left(chunk) => chunk.pos
      case Right(terrain) => terrain.pos
    }
    if (cs.isEmpty)
      return this
    val removed = this -- cs.map(pos)
    new SWorld(time, res,
      removed.chunks ++ cs.map(c => pos(c) -> c),
      chunkDomain, terrainDomain,
      removed.active ++ cs.filter({
        case Left(chunk) => chunk.isActive
        case _ => false
      }).map(pos),
      removed.renderable ++ cs.filter({
        case Left(chunk) => chunk.isRenderable
        case _ => false
      }).map(pos),
      removed.bbox ++ cs.flatMap(_.left.toOption.map(_.pos)))
  }

  def addChunks(cs: Seq[Chunk]): SWorld = this ++ (cs map (Left(_)))
  def addTerrains(ts: Seq[Terrain]): SWorld = this ++ (ts map (Right(_)))

  def +(c: Either[Chunk, Terrain]): SWorld = {
    val p = Profiler("world add")
    def pos(c: Either[Chunk, Terrain]): V3I = c match {
      case Left(chunk) => chunk.pos
      case Right(terrain) => terrain.pos
    }
    val removed = this - pos(c)
    try new SWorld(time, res,
      removed.chunks + (pos(c) -> c),
      chunkDomain, terrainDomain,
      c match {
        case Left(chunk) if chunk isActive => removed.active + chunk.pos
        case _ => removed.active
      },
      c match {
        case Left(chunk) if chunk isRenderable => removed.renderable + chunk.pos
        case _ => removed.renderable
      },
      c match {
        case Left(chunk) => removed.bbox + chunk.pos
        case _ => removed.bbox
      }
    )
    finally {
      p.log()
      p.printDisc(1)
    }
  }

  def +(c: Chunk): SWorld = this + Left(c)
  def +(t: Terrain): SWorld = this + Right(t)

  case class WithReplacedChunk(replaced: Chunk) extends World {
    lazy val nchunks = chunks + (replaced.pos -> Left(replaced))

    override def chunkAt(p: V3I): Option[Chunk] = nchunks.get(p).flatMap(LeftOption(_))

    override def terrainAt(p: V3I): Option[Terrain] = SWorld.this.terrainAt(p)

    override def time: Long = SWorld.this.time

    override def res: Int = SWorld.this.res

    override def findEntity(id: EntityID): Option[Entity] = nchunks.values.toStream.flatMap(LeftOption(_)).flatMap(_.entities.get(id)).headOption

    override def boundingBox: (V3I, V3I) = SWorld.this.boundingBox

    override def debugLoadedChunks: Iterable[V3I] = SWorld.this.debugLoadedChunks.toSeq :+ replaced.pos

    override def debugLoadedTerrain: Iterable[V3I] = SWorld.this.debugLoadedTerrain
  }

  /**
    * Integrate the events into the world, assuming the chunks are present, for events which's target modulo mod
    * equals frequency.
    */
  /*
  def integrate(events: Map[V3I, Seq[ChunkEvent]], mod: Int, freq: V3I): (SWorld, Seq[UpdateEffect]) = {
    val (updated: Seq[Chunk], accumulated: Seq[Seq[UpdateEffect]]) =
      events.filterKeys(_ % mod == freq).map({
        case (p, group) => group.foldLeft((chunks(p).left.get, Seq.empty[UpdateEffect])) {
          case ((chunk, accumulator), event) => {
            val (updatedChunk, newEffects) = event(chunk, WithReplacedChunk(chunk))
            (updatedChunk, accumulator ++ newEffects)
          }
        }
      }).toSeq.unzip
    (this addChunks updated, accumulated.flatten)
  }

  /**
    * Integrate the event seq into the world on 8 different frequencies in an <0, 0, 0> through <1, 1, 1> range to
    * ensure that chunk transformations are always visible to adjacent chunks while they're being transformed in
    * parallel.
    */
  def integrate(events: Seq[ChunkEvent]): (SWorld, Seq[UpdateEffect]) = {
    val grouped = events.groupBy(_.target)
    Origin.untilAsSeq(V3I(2, 2, 2)).foldLeft((this, Seq.empty[UpdateEffect])) {
      case ((world, accumulator), freq) => {
        val (updatedWorld, newEffects) = world.integrate(grouped, 2, freq)
        (updatedWorld, accumulator ++ newEffects)
      }}
  }
  */
  def integrate(_events: Seq[ChunkEvent]): (SWorld, Seq[UpdateEffect]) = {
    var world = this
    val events = _events.toBuffer
    val effects = new mutable.ArrayBuffer[UpdateEffect]

    while (events.nonEmpty) events.remove(events.size - 1) match {
      case UniChunkEvent(target, func, id) =>
        val (c: Chunk, e: Seq[UpdateEffect]) = func(world.chunkAt(target).get, world)
        val (ec, eo) = e.partition(_.effectType == ChunkEvent)

        world += c
        events ++= ec.map(_.asInstanceOf[ChunkEvent])
        effects ++= eo
      case MultiChunkEvent(target, func, id) =>
        val in: Map[V3I, Chunk] = target.toSeq.map(p => (p, world.chunkAt(p).get)).toMap
        val (c: Map[V3I, Chunk], e: Seq[UpdateEffect]) = func(in, world)
        val (ec, eo) = e.partition(_.effectType == ChunkEvent)

        world = world.addChunks(c.values.toSeq)
        events ++= ec.map(_.asInstanceOf[ChunkEvent])
        effects ++= eo
    }

    (world, effects)
  }

  /**
    * Increment the time
    */
  def incrTime: SWorld = {
    new SWorld(time + 1, res, chunks, chunkDomain, terrainDomain, active, renderable, bbox)
  }

  def setDomain(newChunkDomain: V3ISet, newTerrainDomain: V3ISet): SWorld = {
    new SWorld(time, res, chunks, newChunkDomain, newTerrainDomain, active, renderable, bbox)
  }

  def seed(p: V3I): MRNG.Seed =
    (time.hashCode().toLong << 32) | hashCode().toLong


  /**
    * Get all effects from chunks with complete terrain
    */
  def effects(dt: Float): Seq[UpdateEffect] = {
    active.toSeq.map(chunks(_).left.get).flatMap(chunk => chunk.update(this)(new MRNG(seed(chunk.pos))))
  }
}

/**
  * Manages an asynchronous continuum of infinite worlds.
  */
class Infinitum(res: Int, save: AsyncSave, dt: Float) {

  @volatile private var history = SortedMap(0L -> new SWorld(0, res, Map.empty, V3ISet.empty, V3ISet.empty, Set.empty, Set.empty, BBox.empty))

  private implicit val chunkFulfill = new FulfillmentContext[V3I, Chunk]
  private implicit val terrainFulfill = new FulfillmentContext[V3I, Terrain]
  private implicit val executor = UniExecutor.getService

  type LoadID = UUID
  private val chunkLoadQueue = new ConcurrentLinkedQueue[(Chunk, LoadID)]
  private var chunkLoadMap = Map.empty[V3I, LoadID]
  private var terrainLoadQueue = new ConcurrentLinkedQueue[(Terrain, LoadID)]
  private var terrainLoadMap = Map.empty[V3I, LoadID]

  // TODO: save to db
  //private var pendingEvents = Map.empty[V3I, Seq[ChunkEvent]]
  private val requestedQueue = new ConcurrentLinkedQueue[World => Seq[ChunkEvent]]
  private val pendedQueue = new ConcurrentLinkedQueue[ChunkEvent]

  def apply(t: Long): Option[SWorld] = history.get(t)
  def apply(): SWorld = history.last._2

  def loading: Set[V3I] = chunkLoadMap.keySet

  def finalSave(): Fut[Unit] = {
    save.close(this().chunks.flatMap({
      case (p, Left(chunk)) => Some(p -> chunk)
      case _ => None
    }))
  }

  /**
    * Update the world, and return the effects.
    * This concurrent, imperative logic is probably the most complicated part of this game, so let's Keep It Simple Sweety.
   */
  def update(chunkDomain: V3ISet, terrainDomain: V3ISet, externalEvents: Seq[UpdateEffect] = Seq.empty): Map[UpdateEffectType, Seq[UpdateEffect]] = {
    var world = this()

    val p = Profiler("main update")

    // downgrade chunks that left the chunk domain
    val toDowngrade: Seq[V3I] = (world.chunkDomain -- chunkDomain).toSeq
    save.push(toDowngrade.flatMap(world.chunks.get).flatMap(_.left.toOption))
    world = world.downgrade(toDowngrade)
    chunkFulfill.remove(toDowngrade)
    chunkLoadMap --= toDowngrade

    // remove terrain that left the terrain domain
    val toRemove: Seq[V3I] = (world.terrainDomain -- terrainDomain).toSeq
    world --= toRemove
    terrainFulfill.remove(toRemove)
    terrainLoadMap --= toRemove

    p.log()

    // pull chunk and terrain futures from save to create load futures
    val (chunkLoadFuts, terrainLoadFuts) = save.pull((chunkDomain -- world.chunkDomain).toSeq, (terrainDomain -- world.terrainDomain).toSeq)
    for ((p, fut) <- chunkLoadFuts) {
      val loadID = UUID.randomUUID()
      chunkLoadMap += p -> loadID
      fut.onComplete(() => chunkLoadQueue.add(fut.query.get -> loadID))
    }
    for ((p, fut) <- terrainLoadFuts) {
      val loadID = UUID.randomUUID()
      terrainLoadMap += p -> loadID
      fut.onComplete(() => terrainLoadQueue.add(fut.query.get -> loadID))
    }

    // set the new domain
    world = world.setDomain(chunkDomain, terrainDomain)

    p.log()

    // TODO: should we pend other effects?

    // get effects and begin to accumulate events
    val effects: Map[UpdateEffectType, Seq[UpdateEffect]] =
      (world.effects(dt) ++ externalEvents).groupBy(_.effectType).withDefaultValue(Seq.empty)
    var events: Seq[ChunkEvent] = effects(ChunkEvent).map(_.asInstanceOf[ChunkEvent])

    p.log()

    // pull loaded chunks from the queue and add them to the world if they're valid, also accumulate pending events
    val pullLimit = 15 milliseconds

    val lp = Profiler("chunk load")
    val cLoadTimer = Timer.start
    while (cLoadTimer.elapsed < pullLimit && !chunkLoadQueue.isEmpty) {
      val (chunk, loadID) = chunkLoadQueue.remove()
      if (chunkLoadMap.get(chunk.pos).contains(loadID)) {
        chunkLoadMap -= chunk.pos
        world += chunk
        chunkFulfill.put(chunk.pos, chunk)
        terrainFulfill.put(chunk.pos, chunk.terrain)
        /*
        if (pendingEvents contains chunk.pos) {
          events ++= pendingEvents(chunk.pos)
          pendingEvents -= chunk.pos
        }
        */
      }
      lp.log()
    }
    lp.printDisc(pullLimit.toMillis)

    val tp = Profiler("terrain load")
    val tLoadTimer = Timer.start
    while (tLoadTimer.elapsed < pullLimit && !terrainLoadQueue.isEmpty) {
      val ltp = Profiler("single terrain load")
      val (terrain, loadID) = terrainLoadQueue.remove()
      ltp.log()
      if (!world.chunks.contains(terrain.pos) && terrainLoadMap.get(terrain.pos).contains(loadID)) {
        ltp.log()
        terrainLoadMap -= terrain.pos
        ltp.log()
        world += terrain
        ltp.log()
        terrainFulfill.put(terrain.pos, terrain)
      }
      ltp.log()
      ltp.printDisc(1)
      tp.log()
    }
    tp.printDisc(pullLimit.toMillis)

    p.log()

    // pull pended events from the queue
    while (!pendedQueue.isEmpty) {
      events +:= pendedQueue.remove()
    }

    p.log()

    // pull requested values from the queue and transform them into events, accumulating them
    while (requestedQueue.size > 0) {
      events ++= requestedQueue.remove()(world)
    }

    p.log()

    var specialEffects: Map[UpdateEffectType, Seq[UpdateEffect]] = effects - ChunkEvent

    // recursively modifies the world and specialEffects
    @tailrec def applyEvents(eventsIn: Seq[ChunkEvent]): Unit = {
      var events = eventsIn
      // partition events by whether they can be integrated immediately
      val (integrateNow: Seq[ChunkEvent], integrateLater: Seq[ChunkEvent]) = {
        def ready(p: V3I): Boolean = world.chunks.get(p).flatMap(_.left.toOption).isDefined
        events partition {
          case UniChunkEvent(target, _, _) => ready(target)
          case MultiChunkEvent(target, _, _) => target.forall(ready)
        }
      }
      //val (integrateNow: Seq[ChunkEvent], integrateLater: Seq[ChunkEvent]) =
      //  events.partition(event => world.chunks.get(event.target).flatMap(_.left.toOption).isDefined)

      // make the events that cannot be integrated immediately, be added to the queue upon being ready
      integrateLater foreach {
        case event@UniChunkEvent(target, _, _) =>
          FulfillFut[V3I, Chunk](target).onComplete(() => pendedQueue.add(event))
        case event@MultiChunkEvent(target, _, _) =>
          PromiseFold(target.toSeq.map(p => FulfillFut[V3I, Chunk](p))).onComplete(() => pendedQueue.add(event))
      }

      // add the events that can't be immediately integrated to the pending event sequence
      /*
      for ((key, seq) <- integrateLater.groupBy(_.target)) {
        pendingEvents = pendingEvents.updated(key, pendingEvents.getOrElse(key, Seq.empty) ++ seq)
      }
      */

      // integrate the accumulated events into the world
      val (integrated, newEffects) = world.integrate(integrateNow)
      world = integrated

      // update the chunk fulfiller with the updated chunks
      // TODO: do this once per loop, and optimize the chunk set getting
      /*
      for (p <- integrateNow.map(_.target).distinct) {
        chunkFulfill.put(p, world.chunks(p).left.get)
        terrainFulfill.put(p, world.chunks(p).left.get.terrain)
      }
      */
      // update the fulfillment contexts
      for (p <- integrateNow.flatMap({
        case UniChunkEvent(target, _, _) => Seq(target)
        case MultiChunkEvent(target, _, _) => target
      }).distinct) {
        chunkFulfill.put(p, world.chunks(p).left.get)
        terrainFulfill.put(p, world.chunks(p).left.get.terrain)
      }

      // group
      val newEffectsGrouped = newEffects.groupBy(_.effectType)
      specialEffects = MergeBinned(specialEffects, newEffectsGrouped - ChunkEvent)


      // recurse
      if (newEffectsGrouped.getOrElse(ChunkEvent, Seq.empty).nonEmpty)
        applyEvents(newEffectsGrouped(ChunkEvent).map(_.asInstanceOf[ChunkEvent]))
    }

    // recursively apply the events
    applyEvents(events)

    p.log()

    // process request events
    val toFutPack = WEval.EvalAsync(UniExecutor.getService, chunkFulfill, terrainFulfill)
    for (make <- specialEffects.getOrElse(MakeRequest, Seq.empty).map(_.asInstanceOf[MakeRequest[_]])) {
      val request: Request[_] = make.request
      val fut: Fut[Any] = new AsyncEval(request.eval)().fut(WEval.input, toFutPack)
      fut.onComplete(() => {
        val result: Requested = new Requested(request.id, fut.query.get)
        requestedQueue.add(world => make.onComplete(result, world, new MRNG(ThreadLocalRandom.current.nextLong())))
      })
    }

    p.log()

    // increment time
    world = world.incrTime

    // add to history
    history += world.time -> world

    // prune history
    history = history.rangeImpl(Some(history.lastKey - 20), None)

    p.log()
    //p.printDisc(50)

    // return the accumulated special effects, with default values
    specialEffects.withDefaultValue(Seq.empty)
  }

}