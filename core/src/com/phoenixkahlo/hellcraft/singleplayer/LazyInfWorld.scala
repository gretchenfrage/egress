package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent, World}
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.graphics.{RenderableFactory, ResourcePack}
import com.phoenixkahlo.hellcraft.infinitetest.HashCacheWorld
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.serial.save.WorldSave

import scala.collection.SortedSet
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * The chunks are all the chunks that are loaded into the world.
  * The futures are all the chunks that are being loaded into the world.
  * The border is the buffer zone of chunks not to interact with unless absolutely necessary.
  * The active set is the set of chunks with entities in them, except the border set.
  * The meshable set is the set of chunks that are meshable, except the border set.
  * The entity position hints don't need to be complete or correct, they are hints to improve performance.
  * The union of the active and meshable sets forms the renderable set, which is to be rendered.
  */
class LazyInfWorld(
                    val save: AsyncSave,
                    override val time: Long,
                    val chunks: Map[V3I, Chunk],
                    val futures: Map[V3I, Future[Chunk]],
                    val border: Set[V3I],
                    val active: Set[V3I],
                    val meshable: Set[V3I],
                    @volatile private var entityPosHints: Map[Any, V3I]
                  ) extends World {

  private val renderable = (active ++ meshable).toSeq.map(chunks(_))
  private val cache = new ThreadLocal[Chunk]

  override def chunkIsDefinedAt(p: V3I): Boolean = {
    chunks contains p
  }

  override def chunkAt(p: V3I): Option[Chunk] = {
    val cached = cache.get
    if (cached != null && cached.pos == p) Some(cached)
    else chunks.get(p)
  }

  def strongChunkAt(p: V3I): Chunk = {
    val cached = cache.get
    if (cached != null && cached.pos == p) cached
    chunks.getOrElse(p, Await.result(futures.getOrElse(p, save.pull(Seq(p))(p)), Duration.Inf))
  }

  override def findEntity(id: EntityID): Option[Entity] = {
    entityPosHints.get(id).flatMap(chunkAt).flatMap(_.entities.get(id)) match {
      case entity if entity isDefined => entity
      case None =>
        active.toSeq.map(chunkAt(_).get).flatMap(c => c.entities.get(id).map((c.pos, _))).headOption match {
          case Some((p, entity)) =>
            entityPosHints = entityPosHints.updated(entity.id, p)
            Some(entity)
          case None => None
        }
    }
  }

  def loadify: LazyInfWorld = {
    // extract the completed futures
    val added: Map[V3I, Chunk] =
      futures.values.filter(_.isCompleted).map(Await.result(_, Duration.Zero)).map(c => (c.pos, c)).toMap
    // compute the new map of chunks
    val newChunks: Map[V3I, Chunk] = chunks ++ added
    // compute the new map of futures
    val newFutures = futures -- added.keySet
    // compute the new and removed border
    val borderGrouped: Map[Boolean, Set[V3I]] =
      (border ++ added.keySet).groupBy(_.touching.forall(newChunks.contains))
    val newNotBorder: Set[V3I] = borderGrouped.getOrElse(true, Set.empty)
    val newBorder: Set[V3I] = borderGrouped.getOrElse(false, Set.empty)
    // compute the new active set
    val newActive = active ++ newNotBorder.toSeq.filter(newChunks(_).entities.nonEmpty)
    // compute the new meshable set
    val newMeshable = meshable ++ newNotBorder.toSeq.filter(newChunks(_).isMeshable(newChunks.get))
    // construct
    new LazyInfWorld(save, time, newChunks, newFutures, newBorder, newActive, newMeshable, entityPosHints)
  }

  def updateLoaded(target: Seq[V3I]): LazyInfWorld = {
    // unload chunks
    val unload: Seq[Chunk] = (chunks.keySet -- target).toSeq.flatMap(chunks.get)
    save.push(unload)
    val unloaded = unload.map(_.pos)
    // load chunks
    val load: Map[V3I, Future[Chunk]] = save.pull((target.toSet -- chunks.keys).toSeq)
    // construct
    new LazyInfWorld(save, time, chunks -- unloaded, futures ++ load, border -- unloaded, active -- unloaded,
      meshable -- unloaded, entityPosHints)
  }

  def pushToSave(): Future[Unit] = {
    Future {
      for (future <- save.push(chunks))
        Await.result(future, Duration.Inf)
      ()
    }(ExecutionContext.global)
  }

  def integrate(events: Seq[ChunkEvent]): LazyInfWorld = {
    // produce updated chunks
    val updated: Map[V3I, Chunk] =
      events.groupBy(_.target).par.map({
        case (p, group) => group.foldLeft(strongChunkAt(p)) { case (c, e) => e(c) }
      }).map(c => (c.pos, c)).toMap.seq
    // compute new chunk map
    val newChunks: Map[V3I, Chunk] = chunks ++ updated
    // compute which chunks were added
    val added: Set[V3I] = updated.keySet.filterNot(chunks.contains)
    // compute which futures were removed
    val newFutures = futures -- added
    // compute the new border
    val borderGrouped: Map[Boolean, Set[V3I]] =
      (border ++ added).groupBy(_.touching.forall(newChunks.contains))
    val newBorder: Set[V3I] = borderGrouped.getOrElse(false, Set.empty)
    val updatedNotBorder: Set[V3I] = updated.keySet ++ borderGrouped.getOrElse(true, Set.empty)
    // compute the new active set
    val newActive = active ++ updatedNotBorder.toSeq.filter(newChunks(_).entities.nonEmpty)
    // compute the new meshable set
    val newMeshable = meshable ++ updatedNotBorder.toSeq.filter(newChunks(_).isMeshable(newChunks.get))
    // construct
    new LazyInfWorld(save, time, newChunks, newFutures, newBorder, newActive, newMeshable, entityPosHints)
  }

  def incrTime: LazyInfWorld =
    new LazyInfWorld(save, time + 1, chunks, futures, border, active, meshable, entityPosHints)

  def update: LazyInfWorld = {
    val loadified = loadify
    val events = loadified.chunks.keys.toSeq.par.flatMap(loadified.chunkAt).flatMap(_.update(loadified)).seq
    loadified.integrate(events).incrTime
  }

  def renderables(resources: ResourcePack): Seq[RenderableFactory] = {
    renderable.flatMap(_.renderables(resources, this))
  }

}
