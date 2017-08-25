package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID
import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingQueue}

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.graphics.`new`.RenderUnit
import com.phoenixkahlo.hellcraft.math.{Origin, V3I}
import com.phoenixkahlo.hellcraft.oldsingleplayer.LazyInfWorld
import com.phoenixkahlo.hellcraft.util.threading.{Fut, Promise, UniExecutor}

import scala.collection.{SortedMap, immutable}

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
      removed.state3 ++ grouped(Quads)
    )
  }

  def +(c: Chunk): SWorld = {
    val removed = this - c.pos
    val group = c.terrain.terrainType
    new SWorld(time, res,
      removed.chunks + (c.pos -> c),
      if (group == Densities) removed.state1 + c.pos else removed.state1,
      if (group == Vertices) removed.state2 + c.pos else removed.state2,
      if (group == Quads) removed.state3 + c.pos else removed.state3
    )
  }

  /**
    * The chunks which's terrain can be upgraded with this world.
    */
  def upgradeable: Seq[Chunk] = {
    val up1 = state1.toSeq.map(chunks(_)).filter(_.terrain match { case d: Densities => d.canUpgrade(this) })
    val up2 = state2.toSeq.map(chunks(_)).filter(_.terrain match { case v: Vertices => v.canUpgrade(this) })
    up1 ++ up2
  }

  /**
    * Integrate the events into the world, assuming the chunks are present, for events which's target modulo mod
    * equals frequency.
    */
  def integrate(events: Map[V3I, Seq[ChunkEvent]], mod: Int, freq: V3I): SWorld = {
    val updated = events.filterKeys(_ % mod == freq).par.map({
      case (p, group) => group.foldLeft(chunks(p)) { case (c, e) => e(c) }
    }).toSeq.seq
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
    * Get all events from chunks with complete terrain
    */
  def events(dt: Float): Seq[ChunkEvent] = {
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

  private val upgradeQueue = new ConcurrentLinkedQueue[(Terrain, UpgradeID)]
  private var upgradeMap = Map.empty[V3I, UpgradeID]

  def apply(t: Long): Option[SWorld] = history.get(t)
  def apply(): SWorld = history.last._2

  def update(loadTarget: Set[V3I]): Unit = {
    var world = this()

    // push chunks to save
    val toUnload = (world.chunks.keySet -- loadTarget).toSeq.map(world.chunks(_))
    save.push(toUnload)
    // remove them from the world
    world --= toUnload.map(_.pos)

    // pull chunk futures from save to create load futures
    val loadFuts = save.pull((loadTarget -- world.chunks.keySet -- loadMap.keySet).toSeq)
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
        }
        upgradeQueue.add(upgraded -> upgradeID)
      })
    }

    // pull loaded chunks from the queue and add them to the world if they're valid
    while (loadQueue.size > 0) {
      val (chunk, loadID) = loadQueue.remove()
      if (loadMap.get(chunk.pos).contains(loadID)) {
        loadMap -= chunk.pos
        world += chunk
      }
    }

    // get the events from the world
    var events = world.events(dt)

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

    // integrate the accumulated events into the world
    world = world.integrate(events)

    // increment time
    world = world.incrTime

    // append to history
    history += world.time -> world
  }


}

/*
class Infinitum(res: Int, save: AsyncSave, dt: Float) {

  @volatile var history = SortedMap(0L -> new InfWorld(0, res, Map.empty, Set.empty, Map.empty))
  history += (0L -> new InfWorld(0, res, Map.empty, Set.empty, Map.empty))

  val loadQueue = new LinkedBlockingQueue[Chunk]

  def apply(time: Long): Option[InfWorld] = history.get(time)

  def apply(): InfWorld = history.last._2

  def update(loadTarget: Set[V3I]): Unit = {
    var world = this()

    // push chunks to save
    val unload: Seq[Chunk] = (world.chunks.keySet -- loadTarget).toSeq.map(world.chunks(_))
    save.push(unload)

    // create load futures
    val toLoad: Map[V3I, Fut[Chunk]] = save.pull((loadTarget -- world.chunks.keySet).toSeq)
    for (fut <- toLoad.values) fut.onComplete(() => loadQueue.add(fut.query.get))

    // accumulate from the queue
    var loaded = Seq.empty[Chunk]
    while (loadQueue.size > 0) loaded +:= loadQueue.remove()

    // filter out chunks that aren't being loaded
    loaded = loaded.filter(loadTarget contains _.pos)

    // integrate them into the world
    var (newWorld: InfWorld, toVertex: Seq[Chunk], toQuad: Seq[Chunk]) = world ++ loaded
    world = newWorld

    // get events
    val events = world.complete.toSeq.map(world.chunks(_)).flatMap(_.update(world, dt)).groupBy(_.target)

    // integrate
    val m = 2
    for (fre <- Origin until V3I(m, m, m)) {
      val (integrated, newToVertex, newToQuad) = world.integrate(events, m, fre)
      world = integrated
      toVertex ++= newToVertex
      toQuad ++= newToQuad
    }

    // create vertex futures
    for (chunk <- toVertex.filter(_.terrain.asInstanceOf[Densities].canUpgrade(world))) {
      val fut = Fut(chunk.updateTerrain(chunk.terrain.asInstanceOf[Densities].upgrade(world).get),
        UniExecutor.exec(chunk.pos * 16 + V3I(8, 8, 8)))
      fut.onComplete(() => loadQueue.add(fut.query.get))
    }

    // create quad futures
    for (chunk <- toQuad.filter(_.terrain.asInstanceOf[Vertices].canUpgrade(world))) {
      val fut = Fut(chunk.updateTerrain(chunk.terrain.asInstanceOf[Vertices].upgrade(world).get),
        UniExecutor.exec(chunk.pos * 16 + V3I(8, 8, 8)))
      fut.onComplete(() => loadQueue.add(fut.query.get))
    }

    // increment time and append to history
    world = world.incrTime
    history += (world.time -> world)
  }

}
 */

/*
class InfWorld(
                override val time: Long,
                override val res: Int,
                val chunks: Map[V3I, Chunk],
                val complete: Set[V3I],
                @volatile private var entityPosHints: Map[UUID, V3I]
              ) extends World {

  private val cache = new ThreadLocal[Chunk]

  override def chunkAt(p: V3I): Option[Chunk] = {
    (Option(cache.get).filter(_.pos == p) ++ chunks.get(p)).headOption
  }

  override def findEntity(id: EntityID): Option[Entity] = {
    entityPosHints.get(id).flatMap(chunkAt).flatMap(_.entities.get(id)) match {
      case entity if entity isDefined => entity
      case None =>
        chunks.values.flatMap(c => c.entities.get(id).map(c.pos -> _)).headOption match {
          case Some((p, entity)) =>
            entityPosHints += (entity.id -> p)
            Some(entity)
          case None => None
        }
    }
  }

  private def add(added: Seq[Chunk]): (InfWorld, Seq[Chunk], Seq[Chunk]) = {
    val sorted: Map[TerrainType, Seq[Chunk]] = added.groupBy(_.terrain.terrainType)
    val newWorld = new InfWorld(time, res, chunks ++ added.map(c => c.pos -> c),
      complete ++ sorted.getOrElse(Quads, Seq.empty).map(_.pos),
      entityPosHints)
    (newWorld, sorted.getOrElse(Densities, Seq.empty), sorted.getOrElse(Vertices, Seq.empty))
  }

  /**
    * @return the updated world, the new chunks in the density state, and the new chunks in the vertex state
    */
  def ++(added: Seq[Chunk]): (InfWorld, Seq[Chunk], Seq[Chunk]) =
    this -- added.map(_.pos) add added

  def --(ps: Seq[V3I]): InfWorld = {
    new InfWorld(time, res, chunks -- ps, complete -- ps, entityPosHints)
  }

  /**
    * Assumes that all the targeted chunks are present
    * @return the updated world, the new chunks in the density state, and the new chunks in the vertex state
    */
  def integrate(events: Map[V3I, Seq[ChunkEvent]], mod: Int, fre: V3I): (InfWorld, Seq[Chunk], Seq[Chunk]) = {
    val updated: Seq[Chunk] = events.filterKeys(_ % mod == fre).par.map({
      case (p, group) => group.foldLeft(chunks(p)) { case (c, e) => e(c) }
    }).toSeq.seq
    this -- updated.map(_.pos) ++ updated
  }

  def incrTime: InfWorld = {
    new InfWorld(time + 1, res, chunks, complete, entityPosHints)
  }

  def renderables(resources: ResourcePack): Seq[RenderUnit] = {
    complete.map(chunks(_)).flatMap(_.renderables(resources, this)).toSeq
  }

}

class Infinitum(res: Int, save: AsyncSave, dt: Float) {

  @volatile var history = SortedMap(0L -> new InfWorld(0, res, Map.empty, Set.empty, Map.empty))
  history += (0L -> new InfWorld(0, res, Map.empty, Set.empty, Map.empty))

  val loadQueue = new LinkedBlockingQueue[Chunk]

  def apply(time: Long): Option[InfWorld] = history.get(time)

  def apply(): InfWorld = history.last._2

  def update(loadTarget: Set[V3I]): Unit = {
    var world = this()

    // push chunks to save
    val unload: Seq[Chunk] = (world.chunks.keySet -- loadTarget).toSeq.map(world.chunks(_))
    save.push(unload)

    // create load futures
    val toLoad: Map[V3I, Fut[Chunk]] = save.pull((loadTarget -- world.chunks.keySet).toSeq)
    for (fut <- toLoad.values) fut.onComplete(() => loadQueue.add(fut.query.get))

    // accumulate from the queue
    var loaded = Seq.empty[Chunk]
    while (loadQueue.size > 0) loaded +:= loadQueue.remove()

    // filter out chunks that aren't being loaded
    loaded = loaded.filter(loadTarget contains _.pos)

    // integrate them into the world
    var (newWorld: InfWorld, toVertex: Seq[Chunk], toQuad: Seq[Chunk]) = world ++ loaded
    world = newWorld

    // get events
    val events = world.complete.toSeq.map(world.chunks(_)).flatMap(_.update(world, dt)).groupBy(_.target)

    // integrate
    val m = 2
    for (fre <- Origin until V3I(m, m, m)) {
      val (integrated, newToVertex, newToQuad) = world.integrate(events, m, fre)
      world = integrated
      toVertex ++= newToVertex
      toQuad ++= newToQuad
    }

    // create vertex futures
    for (chunk <- toVertex.filter(_.terrain.asInstanceOf[Densities].canUpgrade(world))) {
      val fut = Fut(chunk.updateTerrain(chunk.terrain.asInstanceOf[Densities].upgrade(world).get),
        UniExecutor.exec(chunk.pos * 16 + V3I(8, 8, 8)))
      fut.onComplete(() => loadQueue.add(fut.query.get))
    }

    // create quad futures
    for (chunk <- toQuad.filter(_.terrain.asInstanceOf[Vertices].canUpgrade(world))) {
      val fut = Fut(chunk.updateTerrain(chunk.terrain.asInstanceOf[Vertices].upgrade(world).get),
        UniExecutor.exec(chunk.pos * 16 + V3I(8, 8, 8)))
      fut.onComplete(() => loadQueue.add(fut.query.get))
    }

    // increment time and append to history
    world = world.incrTime
    history += (world.time -> world)
  }

}
*/