package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID
import java.util.concurrent.ConcurrentLinkedQueue

import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.graphics.{RenderUnit, ResourcePack}
import com.phoenixkahlo.hellcraft.math.{Ones, Origin, V3F, V3I}
import com.phoenixkahlo.hellcraft.util.collections.MergeBinned
import com.phoenixkahlo.hellcraft.util.threading._

import scala.annotation.tailrec
import scala.collection.SortedMap

/**
  * A purely functional implementation of world, that stores a map of chunks, and three set of chunk indices,
  * which categorize which chunks are in which terrain state.
  */
class SWorld(
              override val time: Long,
              override val res: Int,
              val chunks: Map[V3I, Chunk],
              val complete: Set[V3I],
              val active: Set[V3I],
              val missingTerrSoup: Set[V3I],
              val missingBlockSoup: Set[V3I],
              val min: Option[V3I],
              val max: Option[V3I]
            ) extends World {

  override def chunkAt(p: V3I): Option[Chunk] =
    chunks.get(p)

  override def findEntity(id: EntityID): Option[Entity] =
    chunks.values.toStream.flatMap(_.entities.get(id)).headOption

  override def boundingBox: (V3I, V3I) = (min, max) match {
    case (Some(minP), Some(maxP)) => (minP, maxP)
    case _ => (Origin, Origin)
  }

  private def compBoundingBox: SWorld =
    if (chunks isEmpty)
      new SWorld(time, res, chunks, complete, active, missingTerrSoup, missingBlockSoup, None, None)
    else
      new SWorld(time, res, chunks, complete, active, missingTerrSoup, missingBlockSoup,
        Some(V3I(chunks.keySet.toSeq.map(_.xi).min, chunks.keySet.toSeq.map(_.yi).min, chunks.keySet.toSeq.map(_.zi).min)),
        Some(V3I(chunks.keySet.toSeq.map(_.xi).max, chunks.keySet.toSeq.map(_.yi).max, chunks.keySet.toSeq.map(_.zi).max))
      )

  /**
    * Remove those chunks from this world,
    */
  def --(ps: Seq[V3I]): SWorld = {
    new SWorld(time, res, chunks -- ps, complete -- ps, active -- ps, missingTerrSoup -- ps, missingBlockSoup -- ps, None, None).compBoundingBox
  }

  def -(p: V3I): SWorld = {
    new SWorld(time, res, chunks - p, complete - p, active - p, missingTerrSoup - p, missingBlockSoup - p, None, None).compBoundingBox
  }

  /**
    * Add those chunks to the world.
    */
  def ++(cs: Seq[Chunk]): SWorld = {
    if (cs.isEmpty)
      return this
    val removed = this -- cs.map(_.pos)
    //val (complete, incomplete) = cs.partition(_.terrain.isComplete)
    val complete = cs.filter(_.isComplete)
    val mts = cs.filter(_.terrainSoup.isEmpty)
    val mbs = cs.filter(_.blockSoup.isEmpty)
    new SWorld(time, res,
      removed.chunks ++ cs.map(c => c.pos -> c),
      removed.complete ++ complete.map(_.pos),
      removed.active ++ cs.filter(_ isActive).map(_.pos),
      removed.missingTerrSoup ++ mts.map(_.pos),
      removed.missingBlockSoup ++ mbs.map(_.pos),
      Some(V3I(
        Math.min(cs.map(_.pos.xi).min, min.map(_.xi).getOrElse(Int.MaxValue)),
        Math.min(cs.map(_.pos.yi).min, min.map(_.yi).getOrElse(Int.MaxValue)),
        Math.min(cs.map(_.pos.zi).min, min.map(_.zi).getOrElse(Int.MaxValue))
      )), Some(V3I(
        Math.max(cs.map(_.pos.xi).max, max.map(_.xi).getOrElse(Int.MinValue)),
        Math.max(cs.map(_.pos.yi).max, max.map(_.yi).getOrElse(Int.MinValue)),
        Math.max(cs.map(_.pos.zi).max, max.map(_.zi).getOrElse(Int.MinValue))
      )))
  }

  def +(c: Chunk): SWorld = {
    val removed = this - c.pos
    new SWorld(time, res,
      removed.chunks + (c.pos -> c),
      if (c.isComplete) removed.complete + c.pos else removed.complete,
      if (c.isActive) removed.active + c.pos else removed.active,
      if (c.terrainSoup.isEmpty) removed.missingTerrSoup + c.pos else removed.missingTerrSoup,
      if (c.blockSoup.isEmpty) removed.missingBlockSoup + c.pos else removed.missingBlockSoup,
      Some(V3I(
        Math.min(c.pos.xi, min.map(_.xi).getOrElse(Int.MaxValue)),
        Math.min(c.pos.yi, min.map(_.yi).getOrElse(Int.MaxValue)),
        Math.min(c.pos.zi, min.map(_.zi).getOrElse(Int.MaxValue))
      )), Some(V3I(
        Math.max(c.pos.xi, max.map(_.xi).getOrElse(Int.MinValue)),
        Math.max(c.pos.yi, max.map(_.yi).getOrElse(Int.MinValue)),
        Math.max(c.pos.zi, max.map(_.zi).getOrElse(Int.MinValue))
      )))
  }

  def terrainSoupable: Seq[Chunk] = {
    missingTerrSoup.toSeq.filter(Terrain.canComplete(_, this)).map(chunks(_))
  }

  def blockSoupable: Seq[Chunk] = {
    missingBlockSoup.toSeq.filter(Terrain.canComplete(_, this)).map(chunks(_))
  }

  case class WithReplacedChunk(replaced: Chunk) extends World {
    lazy val nchunks = chunks + (replaced.pos -> replaced)

    override def chunkAt(p: V3I): Option[Chunk] = nchunks.get(p)

    override def time: Long = SWorld.this.time

    override def res: Int = SWorld.this.res

    override def findEntity(id: EntityID): Option[Entity] = nchunks.values.toStream.flatMap(_.entities.get(id)).headOption

    override def boundingBox: (V3I, V3I) = SWorld.this.boundingBox
  }

  /**
    * Integrate the events into the world, assuming the chunks are present, for events which's target modulo mod
    * equals frequency.
    */
  def integrate(events: Map[V3I, Seq[ChunkEvent]], mod: Int, freq: V3I): (SWorld, Seq[UpdateEffect]) = {
    val (updated: Seq[Chunk], accumulated: Seq[Seq[UpdateEffect]]) =
      events.filterKeys(_ % mod == freq).map({
        case (p, group) => group.foldLeft((chunks(p), Seq.empty[UpdateEffect])) {
          case ((chunk, accumulator), event) => {
            val (updatedChunk, newEffects) = event(chunk, WithReplacedChunk(chunk))
            (updatedChunk, accumulator ++ newEffects)
          }
        }
      }).toSeq.unzip
    (this ++ updated, accumulated.flatten)
  }

  /**
    * Integrate the event seq into the world on 8 different frequencies in an <0, 0, 0> through <1, 1, 1> range to
    * ensure that chunk transformations are always visible to adjacent chunks while they're being transformed in
    * parallel.
    */
  def integrate(events: Seq[ChunkEvent]): (SWorld, Seq[UpdateEffect]) = {
    val grouped = events.groupBy(_.target)
    Origin.until(V3I(2, 2, 2)).foldLeft((this, Seq.empty[UpdateEffect])) {
      case ((world, accumulator), freq) => {
        val (updatedWorld, newEffects) = world.integrate(grouped, 2, freq)
        (updatedWorld, accumulator ++ newEffects)
      }}
  }

  /**
    * Increment the time
    */
  def incrTime: SWorld = {
    new SWorld(time + 1, res, chunks, complete, active, missingTerrSoup, missingBlockSoup, min, max)
  }

  /**
    * Get all effects from chunks with complete terrain
    */
  def effects(dt: Float): Seq[UpdateEffect] = {
    active.toSeq.filter(complete(_)).map(chunks(_)).flatMap(_.update(this))
  }

  /**
    * The render units of all chunks
    */
  def renderables(resources: ResourcePack): Seq[RenderUnit] = {
    chunks.values.flatMap(_.renderables(resources, this)).toSeq
  }

}

/**
  * Manages an asynchronous continuum of infinite worlds.
  */
class Infinitum(res: Int, save: AsyncSave, dt: Float) {

  @volatile private var history =
    SortedMap(0L -> new SWorld(0, res, Map.empty, Set.empty, Set.empty, Set.empty, Set.empty, None, None))

  type LoadID = UUID
  type TerrainSoupID = UUID
  type BlockSoupID = UUID

  private val loadQueue = new ConcurrentLinkedQueue[(Chunk, LoadID)]
  private var loadMap = Map.empty[V3I, LoadID]

  private var pendingEvents = Map.empty[V3I, Seq[ChunkEvent]]

  private val terrainSoupQueue = new ConcurrentLinkedQueue[(TerrainSoup, TerrainSoupID)]
  private var terrainSoupMap = Map.empty[V3I, TerrainSoupID]

  private val blockSoupQueue = new ConcurrentLinkedQueue[(BlockSoup, BlockSoupID)]
  private var blockSoupMap = Map.empty[V3I, BlockSoupID]

  def apply(t: Long): Option[SWorld] = history.get(t)
  def apply(): SWorld = history.last._2

  def loading: Set[V3I] = loadMap.keySet

  def finalSave(): Fut[Unit] = {
    PromiseFold(save.close(this().chunks))
    /*
    Promise(() => {
      save.close(this().chunks).foreach(_.await)
    }, AsyncExecutor.global.execute)
    */
  }

  /**
    * Update the world, and return the effects.
    * This concurrent, imperative logic is probably the most complicated part of this game, so let's Keep It Simple Sweety.
   */
  def update(loadTarget: Set[V3I], externalEvents: Seq[UpdateEffect] = Seq.empty):
      Map[UpdateEffectType, Seq[UpdateEffect]] = {
    var world = this()

    // push chunks to save
    val toUnload = (world.chunks.keySet -- loadTarget).toSeq.map(world.chunks(_))
    save.push(toUnload)
    // remove them from the world
    world --= toUnload.map(_.pos)

    // pull chunk futures from save to create load futures
    val loadFuts: Map[V3I, Fut[Chunk]] = save.pull((loadTarget -- world.chunks.keySet -- loadMap.keySet).toSeq)
    for ((p, fut) <- loadFuts) {
      val loadID = UUID.randomUUID()
      loadMap += p -> loadID
      fut.onComplete(() => loadQueue.add(fut.query.get -> loadID))
    }

    // create upgrade futures
    val upgradeWith = world
    // for terrain soup
    val terrainSoupable = world.terrainSoupable.filterNot(terrainSoupMap contains _.pos)
    for (chunk <- terrainSoupable) {
      val upgradeID = UUID.randomUUID()
      terrainSoupMap += chunk.pos -> upgradeID
      UniExecutor.exec(chunk.pos * 16 + V3I(8, 8, 8))(() => {
        val soup = TerrainSoup(chunk.terrain, upgradeWith).get
        terrainSoupQueue.add(soup -> upgradeID)
      })
    }
    // for block soup
    val blockSoupable = world.blockSoupable.filterNot(blockSoupMap contains _.pos)
    for (chunk <- blockSoupable) {
      val upgradeID = UUID.randomUUID()
      blockSoupMap += chunk.pos -> upgradeID
      UniExecutor.exec(chunk.pos * 16 + V3I(8, 8, 8))(() => {
        val soup = BlockSoup(chunk.terrain, upgradeWith).get
        blockSoupQueue.add(soup -> upgradeID)
      })
    }

    // get effects and begin to accumulate events
    val effects = (world.effects(dt) ++ externalEvents).groupBy(_.effectType).withDefaultValue(Seq.empty)
    var events = effects(ChunkEvent).map(_.asInstanceOf[ChunkEvent])

    // pull loaded chunks from the queue and add them to the world if they're valid, also accumulate pending events
    while (loadQueue.size > 0) {
      val (chunk, loadID) = loadQueue.remove()
      if (loadMap.get(chunk.pos).contains(loadID)) {
        loadMap -= chunk.pos
        world += chunk
        if (pendingEvents contains chunk.pos) {
          events ++= pendingEvents(chunk.pos)
          pendingEvents -= chunk.pos
        }
      }
    }

    var specialEffects = effects - ChunkEvent

    // recursively modifies the world and specialEffects
    @tailrec def applyEvents(eventsIn: Seq[ChunkEvent]): Unit = {
      var events = eventsIn
      
      // pull upgraded chunks from the queue and add update terrain events if they're valid
      // for terrain soup
      while (terrainSoupQueue.size > 0) {
        val (soup, upgradeID) = terrainSoupQueue.remove()
        if (terrainSoupMap.get(soup.pos).contains(upgradeID)) {
          terrainSoupMap -= soup.pos
          events +:= SetTerrainSoup(soup, UUID.randomUUID())
        }
      }
      // for block soup
      while (blockSoupQueue.size > 0) {
        val (soup, upgradeID) = blockSoupQueue.remove()
        if (blockSoupMap.get(soup.pos).contains(upgradeID)) {
          blockSoupMap -= soup.pos
          events +:= SetBlockSoup(soup, UUID.randomUUID())
        }
      }

      // partition events by whether they can be integrated immediately
      val (integrateNow, integrateLater) = events.partition(world.chunks contains _.target)

      // add the events that can't be immediately integrated to the pending event sequence
      for ((key, seq) <- integrateLater.groupBy(_.target)) {
        pendingEvents = pendingEvents.updated(key, pendingEvents.getOrElse(key, Seq.empty) ++ seq)
      }

      // integrate the accumulated events into the world
      val (integrated, newEffects) = world.integrate(integrateNow)
      world = integrated

      // group
      val newEffectsGrouped = newEffects.groupBy(_.effectType)
      specialEffects = MergeBinned(specialEffects, newEffectsGrouped - ChunkEvent)

      // scan for terrain update effects to invalidate update futures
      val invalidateRange = Ones.neg to Ones
      newEffectsGrouped.getOrElse(TerrainChanged, Seq.empty).map(_.asInstanceOf[TerrainChanged].p)
        .foreach(terrainSoupMap -= _)

      // recurse
      if (newEffectsGrouped.getOrElse(ChunkEvent, Seq.empty).nonEmpty)
        applyEvents(newEffectsGrouped(ChunkEvent).map(_.asInstanceOf[ChunkEvent]))
    }

    // recursively apply the events
    applyEvents(events)

    // increment time
    world = world.incrTime

    // add to history
    history += world.time -> world

    // prune history
    history = history.rangeImpl(Some(history.lastKey - 20), None)

    // return the accumulated special effects, with default values
    specialEffects.withDefaultValue(Seq.empty)
  }

}