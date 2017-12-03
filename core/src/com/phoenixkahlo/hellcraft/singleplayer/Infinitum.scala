package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID
import java.util.concurrent.ConcurrentLinkedQueue

import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.core.request.Evalable.ToFutPack
import com.phoenixkahlo.hellcraft.core.request.{Request, Requested}
import com.phoenixkahlo.hellcraft.graphics.{RenderUnit, ResourcePack}
import com.phoenixkahlo.hellcraft.math.{Ones, Origin, V3F, V3I}
import com.phoenixkahlo.hellcraft.util.LeftOption
import com.phoenixkahlo.hellcraft.util.collections.{MergeBinned, V3ISet}
import com.phoenixkahlo.hellcraft.util.debugging.Profiler
import com.phoenixkahlo.hellcraft.util.threading._

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
              val min: Option[V3I],
              val max: Option[V3I]
            ) extends World {

  override def chunkAt(p: V3I): Option[Chunk] =
    chunks.get(p).flatMap(LeftOption(_))

  override def terrainAt(p: V3I): Option[Terrain] =
    chunks.get(p).map({
      case Left(chunk) => chunk.terrain
      case Right(terrain) => terrain
    })

  override def debugLoadedChunks: Iterable[V3I] = chunks.keySet.toSeq.filter(chunks(_).isLeft)

  override def debugLoadedTerrain: Iterable[V3I] = chunks.keySet.toSeq.filter(chunks(_).isRight)

  override def findEntity(id: EntityID): Option[Entity] =
    chunks.values.toStream.flatMap(LeftOption(_)).flatMap(_.entities.get(id)).headOption

  override def boundingBox: (V3I, V3I) = (min, max) match {
    case (Some(minP), Some(maxP)) => (minP, maxP)
    case _ => (Origin, Origin)
  }

  private def compBoundingBox: SWorld =
    if (chunks isEmpty)
      new SWorld(time, res, chunks, chunkDomain, terrainDomain, active, None, None)
    else
      new SWorld(time, res, chunks, chunkDomain, terrainDomain, active,
        Some(V3I(chunks.keySet.toSeq.map(_.xi).min, chunks.keySet.toSeq.map(_.yi).min, chunks.keySet.toSeq.map(_.zi).min)),
        Some(V3I(chunks.keySet.toSeq.map(_.xi).max, chunks.keySet.toSeq.map(_.yi).max, chunks.keySet.toSeq.map(_.zi).max))
      )

  /**
    * Remove those chunks from this world,
    */
  def --(ps: Seq[V3I]): SWorld = {
    new SWorld(time, res, chunks -- ps, chunkDomain, terrainDomain, active -- ps, None, None).compBoundingBox
  }

  def downgrade(ps: Seq[V3I]): SWorld = {
    new SWorld(time, res, ps.foldLeft(chunks)({ case (map, p) => map.get(p) match {
      case Some(Left(chunk)) => map.updated(p, Right(chunk.terrain))
      case _ => map
    }}), chunkDomain, terrainDomain, active -- ps, None, None)
  }

  def -(p: V3I): SWorld = {
    new SWorld(time, res, chunks - p, chunkDomain, terrainDomain, active - p, None, None).compBoundingBox
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
      Some(V3I(
        Math.min(cs.map(pos(_).xi).min, min.map(_.xi).getOrElse(Int.MaxValue)),
        Math.min(cs.map(pos(_).yi).min, min.map(_.yi).getOrElse(Int.MaxValue)),
        Math.min(cs.map(pos(_).zi).min, min.map(_.zi).getOrElse(Int.MaxValue))
      )), Some(V3I(
        Math.max(cs.map(pos(_).xi).max, max.map(_.xi).getOrElse(Int.MinValue)),
        Math.max(cs.map(pos(_).yi).max, max.map(_.yi).getOrElse(Int.MinValue)),
        Math.max(cs.map(pos(_).zi).max, max.map(_.zi).getOrElse(Int.MinValue))
      )))
  }

  def addChunks(cs: Seq[Chunk]): SWorld = this ++ (cs map (Left(_)))
  def addTerrains(ts: Seq[Terrain]): SWorld = this ++ (ts map (Right(_)))

  def +(c: Either[Chunk, Terrain]): SWorld = {
    def pos(c: Either[Chunk, Terrain]): V3I = c match {
      case Left(chunk) => chunk.pos
      case Right(terrain) => terrain.pos
    }
    val removed = this - pos(c)
    new SWorld(time, res,
      removed.chunks + (pos(c) -> c),
      chunkDomain, terrainDomain,
      c match {
        case Left(chunk) if chunk isActive => removed.active + chunk.pos
        case _ => removed.active
      },
      Some(V3I(
        Math.min(pos(c).xi, min.map(_.xi).getOrElse(Int.MaxValue)),
        Math.min(pos(c).yi, min.map(_.yi).getOrElse(Int.MaxValue)),
        Math.min(pos(c).zi, min.map(_.zi).getOrElse(Int.MaxValue))
      )), Some(V3I(
        Math.max(pos(c).xi, max.map(_.xi).getOrElse(Int.MinValue)),
        Math.max(pos(c).yi, max.map(_.yi).getOrElse(Int.MinValue)),
        Math.max(pos(c).zi, max.map(_.zi).getOrElse(Int.MinValue))
      )))
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

  /**
    * Increment the time
    */
  def incrTime: SWorld = {
    new SWorld(time + 1, res, chunks, chunkDomain, terrainDomain, active, min, max)
  }

  def setDomain(newChunkDomain: V3ISet, newTerrainDomain: V3ISet): SWorld = {
    new SWorld(time, res, chunks, newChunkDomain, newTerrainDomain, active, min, max)
  }

  /**
    * Get all effects from chunks with complete terrain
    */
  def effects(dt: Float): Seq[UpdateEffect] = {
    active.toSeq.map(chunks(_).left.get).flatMap(_.update(this))
  }

  /**
    * The render units of all chunks
    */
  def renderables(resources: ResourcePack): Seq[RenderUnit] = {
    chunks.values.flatMap(_.left.toOption).flatMap(_.renderables(resources, this)).toSeq
  }

}

/**
  * Manages an asynchronous continuum of infinite worlds.
  */
class Infinitum(res: Int, save: AsyncSave, dt: Float) {

  @volatile private var history = SortedMap(0L -> new SWorld(0, res, Map.empty, V3ISet.empty, V3ISet.empty, Set.empty, None, None))

  private implicit val chunkFulfill = new FulfillmentContext[V3I, Chunk]
  private implicit val terrainFulfill = new FulfillmentContext[V3I, Terrain]
  private implicit val executor = UniExecutor.getService

  type LoadID = UUID
  private val chunkLoadQueue = new ConcurrentLinkedQueue[(Chunk, LoadID)]
  private var chunkLoadMap = Map.empty[V3I, LoadID]
  private var terrainLoadQueue = new ConcurrentLinkedQueue[(Terrain, LoadID)]
  private var terrainLoadMap = Map.empty[V3I, LoadID]

  // TODO: save to db
  private var pendingEvents = Map.empty[V3I, Seq[ChunkEvent]]
  private val requestedQueue = new ConcurrentLinkedQueue[World => Seq[ChunkEvent]]

  def apply(t: Long): Option[SWorld] = history.get(t)
  def apply(): SWorld = history.last._2

  def loading: Set[V3I] = chunkLoadMap.keySet

  def finalSave(): Fut[Unit] = {
    PromiseFold(save.close(this().chunks.flatMap({
      case (p, Left(chunk)) => Some(p -> chunk)
      case _ => None
    })))
  }

  /**
    * Update the world, and return the effects.
    * This concurrent, imperative logic is probably the most complicated part of this game, so let's Keep It Simple Sweety.
   */
  def update(chunkDomain: V3ISet, terrainDomain: V3ISet, externalEvents: Seq[UpdateEffect] = Seq.empty): Map[UpdateEffectType, Seq[UpdateEffect]] = {
    var world = this()

    val p = Profiler("main update")

    // downgrade chunks that left the chunk domain
    val toDowngrade = (world.chunkDomain -- chunkDomain).toSeq.flatMap(world.chunks.get).flatMap(_.left.toOption)
    save.push(toDowngrade)
    world = world.downgrade(toDowngrade.map(_.pos))
    chunkFulfill.remove(toDowngrade.map(_.pos))

    // remove terrain that left the terrain domain
    val toRemove = world.terrainDomain -- terrainDomain filter (world.chunks contains)
    world --= toRemove.toSeq
    terrainFulfill.remove(toRemove)

    // push chunks to save
    /*
    val toUnload = (world.chunkDomain -- chunkDomain).toSeq.flatMap(world.chunks.get).flatMap(_.left.toOption)
    save.push(toUnload)
    // remove them from the world
    world --= toUnload.map(_.pos)
    // remove them from the context
    chunkFulfill.remove(toUnload.map(_.pos))
    */

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
    val effects: Map[UpdateEffectType, Seq[UpdateEffect]] = (world.effects(dt) ++ externalEvents).groupBy(_.effectType).withDefaultValue(Seq.empty)
    var events: Seq[ChunkEvent] = effects(ChunkEvent).map(_.asInstanceOf[ChunkEvent])

    p.log()

    // pull loaded chunks from the queue and add them to the world if they're valid, also accumulate pending events
    while (chunkLoadQueue.size > 0) {
      val (chunk, loadID) = chunkLoadQueue.remove()
      if (chunkLoadMap.get(chunk.pos).contains(loadID)) {
        chunkLoadMap -= chunk.pos
        world += chunk
        chunkFulfill.put(chunk.pos, chunk)
        if (pendingEvents contains chunk.pos) {
          events ++= pendingEvents(chunk.pos)
          pendingEvents -= chunk.pos
        }
      }
    }
    while (terrainLoadQueue.size > 0) {
      val (terrain, loadID) = terrainLoadQueue.remove()
      if (terrainLoadMap.get(terrain.pos).contains(loadID)) {
        terrainLoadMap -= terrain.pos
        world += terrain
        terrainFulfill.put(terrain.pos, terrain)
      }
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
      val (integrateNow: Seq[ChunkEvent], integrateLater: Seq[ChunkEvent]) =
        events.partition(event => world.chunks.get(event.target).flatMap(_.left.toOption).isDefined)
        //events.partition(world.chunks contains _.target)

      // add the events that can't be immediately integrated to the pending event sequence
      for ((key, seq) <- integrateLater.groupBy(_.target)) {
        pendingEvents = pendingEvents.updated(key, pendingEvents.getOrElse(key, Seq.empty) ++ seq)
      }

      // integrate the accumulated events into the world
      val (integrated, newEffects) = world.integrate(integrateNow)
      world = integrated

      // update the chunk fulfiller with the updated chunks
      // TODO: do this once per loop, and optimize the chunk set getting
      for (p <- integrateNow.map(_.target).distinct) {
        chunkFulfill.put(p, world.chunks(p).left.get)
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
    val toFutPack = ToFutPack(UniExecutor.getService, chunkFulfill, terrainFulfill)
    for (make <- specialEffects.getOrElse(MakeRequest, Seq.empty).map(_.asInstanceOf[MakeRequest[_]])) {
      val request: Request[_] = make.request
      val fut: Fut[Any] = request.eval.toFut(toFutPack)
      fut.onComplete(() => {
        val result: Requested = new Requested(request.id, fut.query.get)
        requestedQueue.add(world => make.onComplete(result, world))
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
    //p.print()

    // return the accumulated special effects, with default values
    specialEffects.withDefaultValue(Seq.empty)
  }

}