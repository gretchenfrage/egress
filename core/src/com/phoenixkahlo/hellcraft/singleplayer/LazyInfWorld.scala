package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID
import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingQueue}

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.graphics.`new`.RenderUnit
import com.phoenixkahlo.hellcraft.math.{Origin, V3I}
import com.phoenixkahlo.hellcraft.oldsingleplayer.LazyInfWorld
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

import scala.collection.{SortedMap, immutable}

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
    this -- added.map(_.pos) ++ added

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

  @volatile var history = SortedMap.empty[Long, InfWorld]
  history += (0L -> new InfWorld(0, res, Map.empty, Set.empty, Map.empty))

  var loading = Set.empty[V3I]
  val loadQueue = new LinkedBlockingQueue[Chunk]

  def apply(time: Long): Option[InfWorld] = history.get(time)

  def apply(): InfWorld = history.last._2

  def update(loadTarget: Iterable[V3I]): Unit = {
    var world = this()

    // push chunks to save
    val unload: Seq[Chunk] = (world.chunks.keySet -- loadTarget).toSeq.map(world.chunks(_))
    save.push(unload)
    loading --= unload.map(_.pos)

    // create load futures
    val toLoad: Map[V3I, Fut[Chunk]] = save.pull((loadTarget.toSet -- (world.chunks.keySet ++ loading)).toSeq)
    loading ++= toLoad.keySet
    for (fut <- toLoad.values) fut.onComplete(() => loadQueue.add(fut.query.get))

    // accumulate from the queue
    var loaded = Seq.empty[Chunk]
    while (loadQueue.size > 0) loaded +:= loadQueue.remove()

    // filter out chunks that aren't being loaded
    loaded = loaded.filter(loading contains _.pos)

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
      loading += chunk.pos
      val fut = Fut(chunk.updateTerrain(chunk.terrain.asInstanceOf[Densities].upgrade(world).get),
        UniExecutor.exec(chunk.pos * 16 + V3I(8, 8, 8)))
      fut.onComplete(() => loadQueue.add(fut.query.get))
    }

    // create quad futures
    for (chunk <- toQuad.filter(_.terrain.asInstanceOf[Vertices].canUpgrade(world))) {
      loading += chunk.pos
      val fut = Fut(chunk.updateTerrain(chunk.terrain.asInstanceOf[Vertices].upgrade(world).get),
        UniExecutor.exec(chunk.pos * 16 + V3I(8, 8, 8)))
      fut.onComplete(() => loadQueue.add(fut.query.get))
    }

    // increment time and append to history
    world = world.incrTime
    history += (world.time -> world)
  }

}
