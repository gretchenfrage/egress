package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID
import java.util.concurrent.ConcurrentLinkedQueue

import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.graphics.{RenderUnit, ResourcePack}
import com.phoenixkahlo.hellcraft.math.{Origin, V3I}
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

import scala.collection.SortedMap

/**
  * A purely functional implementation of world, that stores a map of chunks, and three set of chunk indices,
  * which categorize which chunks are in which terrain state.
  */
class SWorld(
                  override val time: Long,
                  override val res: Int,
                  val chunks: Map[V3I, Chunk],
                  val state1: Set[V3I],
                  val state2: Set[V3I],
                  val state3: Set[V3I]
                  ) extends World {

  override def chunkAt(p: V3I): Option[Chunk] =
    chunks.get(p)

  override def findEntity(id: EntityID): Option[Entity] =
    chunks.values.flatMap(_.entities.get(id)).headOption

  /**
    * Remove those chunks from this world,
    */
  def --(ps: Seq[V3I]): SWorld = {
    new SWorld(time, res, chunks -- ps, state1 -- ps, state2 -- ps, state3 -- ps)
  }

  def -(p: V3I): SWorld = {
    new SWorld(time, res, chunks - p, state1 - p, state2 - p, state3 - p)
  }

  /**
    * Add those chunks to the world.
    */
  def ++(cs: Seq[Chunk]): SWorld = {
    val removed = this -- cs.map(_.pos)
    val grouped = cs.groupBy(_.terrain.terrainType).mapValues(_.map(_.pos)).withDefault(_ => Seq.empty)
    new SWorld(time, res,
      removed.chunks ++ cs.map(c => c.pos -> c),
      removed.state1 ++ grouped(Densities),
      removed.state2 ++ grouped(Vertices),
      removed.state3 ++ grouped(Meshable)
    )
  }

  def +(c: Chunk): SWorld = {
    val removed = this - c.pos
    val group = c.terrain.terrainType
    new SWorld(time, res,
      removed.chunks + (c.pos -> c),
      if (group == Densities) removed.state1 + c.pos else removed.state1,
      if (group == Vertices) removed.state2 + c.pos else removed.state2,
      if (group == Meshable) removed.state3 + c.pos else removed.state3
    )
  }

  /**
    * The chunks which's terrain can be upgraded with this world.
    */
  def upgradeable: Seq[Chunk] = {
    val up1 = state1.toSeq.map(chunks(_)).filter(_.terrain.asInstanceOf[Densities].canUpgrade(this))
    val up2 = state2.toSeq.map(chunks(_)).filter(_.terrain.asInstanceOf[Vertices].canUpgrade(this))
    up1 ++ up2
  }

  /**
    * Integrate the events into the world, assuming the chunks are present, for events which's target modulo mod
    * equals frequency.
    */
  def integrate(events: Map[V3I, Seq[ChunkEvent]], mod: Int, freq: V3I): SWorld = {
    val updated =
      if (useParCollections) {
        events.par.filterKeys(_ % mod == freq).map({
          case (p, group) => group.foldLeft(chunks(p)) { case (c, e) => e(c) }
        }).toSeq.seq
      } else {
        events.filterKeys(_ % mod == freq).map({
          case (p, group) => group.foldLeft(chunks(p)) { case (c, e) => e(c) }
        }).toSeq
      }
    this ++ updated
  }

  /**
    * Integrate the event seq into the world on 8 different frequencies in an <0, 0, 0> through <1, 1, 1> range to
    * ensure that chunk transformations are always visible to adjacent chunks while they're being transformed in
    * parallel.
    */
  def integrate(events: Seq[ChunkEvent]): SWorld = {
    val grouped = events.groupBy(_.target)
    Origin.until(V3I(2, 2, 2)).foldLeft(this) { case (world, freq) => world.integrate(grouped, 2, freq) }
  }

  /**
    * Increment the time
    */
  def incrTime: SWorld = {
    new SWorld(time + 1, res, chunks, state1, state2, state3)
  }

  /**
    * Get all effects from chunks with complete terrain
    */
  def effects(dt: Float): Seq[UpdateEffect] = {
    state3.toSeq.map(chunks(_)).flatMap(_.update(this, dt))
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

  @volatile private var history = SortedMap(0L -> new SWorld(0, res, Map.empty, Set.empty, Set.empty, Set.empty))

  type LoadID = UUID
  type UpgradeID = UUID

  private val loadQueue = new ConcurrentLinkedQueue[(Chunk, LoadID)]
  private var loadMap = Map.empty[V3I, LoadID]

  private var pendingEvents = Map.empty[V3I, Seq[ChunkEvent]]

  private val upgradeQueue = new ConcurrentLinkedQueue[(Terrain, UpgradeID)]
  private var upgradeMap = Map.empty[V3I, UpgradeID]

  def apply(t: Long): Option[SWorld] = history.get(t)
  def apply(): SWorld = history.last._2

  def loading: Set[V3I] = loadMap.keySet

  /**
    * Update the world, and return the effects.
    * This concurrent, imperative logic is probably the most complicated part of this game, so let's Keep It Simple Sweety.
   */
  def update(loadTarget: Set[V3I], externalEvents: Seq[ChunkEvent] = Seq.empty):
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
    val upgradeable = world.upgradeable.filterNot(upgradeMap contains _.pos)
    for (chunk <- upgradeable) {
      val upgradeID = UUID.randomUUID()
      upgradeMap += chunk.pos -> upgradeID
      UniExecutor.exec(chunk.pos * 16 + V3I(8, 8, 8))(() => {
        val upgraded = chunk.terrain match {
          case d: Densities => d.upgrade(upgradeWith).get
          case v: Vertices => v.upgrade(upgradeWith).get
          case q: Meshable => ???
        }
        upgradeQueue.add(upgraded -> upgradeID)
      })
    }

    // get effects and begin to accumulate events
    val effects = world.effects(dt).groupBy(_.effectType).withDefaultValue(Seq.empty)
    var events = effects(ChunkEvent).map(_.asInstanceOf[ChunkEvent]) ++ externalEvents

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

    // scan the events for update terrain events, and use them to invalidate upgrade futures
    val invalidateRange = V3I(-2, -2, -2) to V3I(2, 2, 2)
    events.flatMap({
      case UpdateTerrain(t, _) => invalidateRange.map(_ + t.pos)
      case _ => Seq.empty
    }).foreach(upgradeMap -= _)

    // pull upgraded chunks from the queue and add update terrain events if they're valid
    while (upgradeQueue.size > 0) {
      val (terrain, upgradeID) = upgradeQueue.remove()
      if (upgradeMap.get(terrain.pos).contains(upgradeID)) {
        upgradeMap -= terrain.pos
        events +:= UpdateTerrain(terrain, UUID.randomUUID())
      }
    }

    // partition events by whether they can be integrated immediately
    val (integrateNow, integrateLater) = events.partition(world.chunks contains _.target)

    // add the events that can't be immediately integrated to the pending event sequence
    //pendingEvents ++= integrateLater.groupBy(_.target)
    for ((key, seq) <- integrateLater.groupBy(_.target)) {
      pendingEvents = pendingEvents.updated(key, pendingEvents.getOrElse(key, Seq.empty) ++ seq)
    }

    // integrate the accumulated events into the world
    world = world.integrate(integrateNow)

    // increment time
    world = world.incrTime

    // append to history
    history += world.time -> world

    effects
  }

}