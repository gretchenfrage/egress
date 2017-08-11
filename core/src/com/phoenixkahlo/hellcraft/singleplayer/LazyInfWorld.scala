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

class LazyInfWorld(
                    val save: AsyncSave,
                    override val time: Long,
                    val chunks: Map[V3I, Chunk],
                    val futures: Map[V3I, Future[Chunk]],
                    val active: Set[V3I],
                    @volatile private var entityPosHints: Map[EntityID, V3I]
                  ) extends World {

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
    val added = futures.values.filter(_.isCompleted).map(Await.result(_, Duration.Zero)).map(c => (c.pos, c)).toMap
    val newlyActive = added.filter({ case (_, chunk) => chunk.entities.nonEmpty }).keys
    new LazyInfWorld(save, time, chunks ++ added, futures -- added.keys, active ++ newlyActive, entityPosHints)
  }

  def updateLoaded(target: Seq[V3I]): LazyInfWorld = {
    // unload chunks
    val unload: Seq[Chunk] = (chunks.keySet -- target).toSeq.flatMap(chunks.get)
    save.push(unload)
    // load chunks
    val load: Map[V3I, Future[Chunk]] = save.pull((target.toSet -- chunks.keys).toSeq)
    // construct
    new LazyInfWorld(save, time, chunks -- unload.map(_.pos), futures ++ load, active -- unload.map(_.pos), entityPosHints)
  }

  def pushToSave(): Future[Unit] = {
    Future {
      for (future <- save.push(chunks))
        Await.result(future, Duration.Inf)
      ()
    }(ExecutionContext.global)
  }

  def integrate(events: Seq[ChunkEvent]): LazyInfWorld = {
    // update chunks
    val updated: Map[V3I, Chunk] =
      events.groupBy(_.target).par.map({
        case (p, group) => group.foldLeft(strongChunkAt(p)) { case (c, e) => e(c) }
      }).map(c => (c.pos, c)).toMap.seq
    // compute newly active
    val byActivity: Map[Boolean, Seq[V3I]] = updated.values.groupBy(_.entities.nonEmpty).mapValues(_.toSeq.map(_.pos))
    val newActive = active -- byActivity.getOrElse(false, Seq.empty) ++ byActivity.getOrElse(true, Seq.empty)
    // construct
    new LazyInfWorld(save, time, chunks ++ updated, futures -- updated.keys, newActive, entityPosHints)
  }

  def incrTime: LazyInfWorld =
    new LazyInfWorld(save, time + 1, chunks, futures, active, entityPosHints)

  def update: LazyInfWorld = {
    val loadified = loadify
    val events = loadified.chunks.keys.toSeq.par.flatMap(loadified.chunkAt).flatMap(_.update(loadified)).seq
    loadified.integrate(events).incrTime
  }

  def renderables(resources: ResourcePack): Seq[RenderableFactory] = {
    chunks.values.filter(_.pos.touching.forall(chunks.contains)).flatMap(_.renderables(resources, this)).toSeq
  }

}
