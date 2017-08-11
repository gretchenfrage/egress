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
                    save: AsyncSave,
                    override val time: Long,
                    chunks: Map[V3I, Either[Chunk, Future[Chunk]]]
                  ) extends World {

  private val cache = new ThreadLocal[Chunk]
  @volatile private var entityPosHints: Map[EntityID, V3I] = Map.empty

  override def chunkIsDefinedAt(p: V3I): Boolean =
    chunks.get(p) match {
      case Some(Left(_)) => true
      case Some(Right(future)) if future.isCompleted => true
      case _ => false
    }

  override def chunkAt(p: V3I): Option[Chunk] = {
    val cached = cache.get
    if (cached != null && cached.pos == p) Some(cached)
    val chunk = chunks.get(p) match {
      case Some(Left(chunk)) => Some(chunk)
      case Some(Right(future)) if future.isCompleted => Some(Await.result(future, Duration.Zero))
      case _ => None
    }
    chunk.foreach(cache.set)
    chunk
  }

  def strongChunkAt(p: V3I): Chunk = {
    val cached = cache.get
    if (cached != null && cached.pos == p) cached
    chunks.get(p) match {
      case Some(Left(chunk)) => chunk
      case Some(Right(future)) => Await.result(future, Duration.Inf)
      case None => Await.result(save.pull(Seq(p))(p), Duration.Inf)
    }
  }

  override def findEntity(id: UUID): Option[Entity] = {
    entityPosHints.get(id).flatMap(chunkAt).flatMap(_.entities.get(id)) match {
      case entity if entity isDefined => entity
      case None =>
        chunks.keys.toSeq.flatMap(chunkAt).flatMap(c => c.entities.get(id).map((c.pos, _))).headOption match {
          case Some((p, entity)) =>
            entityPosHints = entityPosHints.updated(entity.id, p)
            Some(entity)
          case None => None
        }
    }
  }

  def updateLoaded(target: Seq[V3I]): LazyInfWorld = {
    // unload chunks
    val unload: Seq[Chunk] = (chunks.keySet -- target).toSeq.flatMap(chunks.get(_) match {
      case Some(Left(chunk)) => Some(chunk)
      case _ => None
    })
    save.push(unload)
    // load chunks
    val load: Map[V3I, Future[Chunk]] = save.pull((target.toSet -- chunks.keys).toSeq)
    // construct new world
    new LazyInfWorld(save, time, chunks ++ load.mapValues(Right(_)))
  }

  def pushToSave(): Future[Unit] = {
    Future {
      val toPush = chunks.keys.toSeq.flatMap(chunks.get(_) match {
        case Some(Left(chunk)) => Some(chunk)
        case _ => None
      })
      for (future <- save.push(toPush))
        Await.result(future, Duration.Inf)
      ()
    } (ExecutionContext.global)
  }

  def integrate(events: SortedSet[ChunkEvent]): LazyInfWorld = {
    val updated = events.groupBy(_.target).par.map({
      case (p, group) => group.foldLeft(strongChunkAt(p)) { case (c, e) => e(c) }
    }).map(c => (c.pos, c)).toMap.seq
    new LazyInfWorld(save, time, chunks ++ updated.mapValues(Left(_)))
  }

  def incrTime: LazyInfWorld =
    new LazyInfWorld(save, time + 1, chunks)

  def update: LazyInfWorld = {
    val events = chunks.keys.toSeq.flatMap(chunkAt).par.flatMap(_.update(this)).seq.to[SortedSet]
    integrate(events).incrTime
  }

  def renderables(resources: ResourcePack): Seq[RenderableFactory] = {
    val loaded: Map[V3I, Chunk] = chunks.keys.toSeq.flatMap(p => chunkAt(p).map((p, _))).toMap
    loaded.values.filter(_.pos.touching.forall(loaded.contains)).flatMap(_.renderables(resources, this)).toSeq
  }


}