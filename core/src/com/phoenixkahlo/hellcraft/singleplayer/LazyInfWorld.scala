package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent, World}
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.graphics.`new`.RenderUnit
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.threading.Fut


class LazyInfWorld(
                  val save: AsyncSave,
                  override val time: Long,
                  val chunks: Map[V3I, Chunk],
                  val futures: Map[V3I, Fut[Chunk]],
                  val border: Set[V3I],
                  val active: Set[V3I],
                  val meshable: Set[V3I],
                  @volatile private var entityPosHints: Map[UUID, V3I]
                  ) extends World {

  private val renderable = (active ++ meshable).toSeq.map(chunks(_))
  private val cache = new ThreadLocal[Chunk]

  override val res = 32

  override def chunkAt(p: V3I): Option[Chunk] = {
    val cached = cache.get
    if (cached != null && cached.pos == p) Some(cached)
    else if (chunks contains p) Some(chunks(p))
    else if (futures contains p) futures(p).query
    else None
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
      futures.values.flatMap(_.query).map(c => (c.pos, c)).toMap
    // compute the new map of chunks
    val newChunks: Map[V3I, Chunk] = chunks ++ added
    // compute the new map of futures
    val newFutures = futures -- added.keySet
    // compute the new and removed border
    val borderGrouped: Map[Boolean, Set[V3I]] =
      (border ++ added.keySet).groupBy(dependencies(_).forall(newChunks.contains))
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
    val load: Map[V3I, Fut[Chunk]] = save.pull((target.toSet -- (chunks.keySet ++ futures.keySet)).toSeq)
    // construct
    new LazyInfWorld(save, time, chunks -- unloaded, futures ++ load, border -- unloaded, active -- unloaded,
      meshable -- unloaded, entityPosHints)
  }

  def ensureAccessible(target: Seq[V3I]): LazyInfWorld = {
    val withFutures = updateLoaded((chunks.keySet ++ futures.keySet ++ dependencies(target)).toSeq)
    target.flatMap(withFutures.futures.get).foreach(_.await)
    withFutures.loadify
  }

  def dependencies(p: V3I): Seq[V3I] =
    V3I(-2, -2, -2).to(V3I(2, 2, 2)).map(_ + p)

  def dependencies(chunks: Seq[V3I]): Seq[V3I] =
    chunks.flatMap(dependencies)

  def pushToSave(): Fut[Unit] = {
    Fut[Unit]({
      for (fut <- save.finalPush(chunks))
        fut.await
      ()
    }, new Thread(_).start())
  }

  def integrate(events: Seq[ChunkEvent]): LazyInfWorld = {
    val accessible = ensureAccessible(events.map(_.target))
    // produce updated chunks
    val updated: Map[V3I, Chunk] =
      events.groupBy(_.target).par.map({
        case (p, group) => group.foldLeft(accessible.chunkAt(p).get) { case (c, e) => e(c) }
      }).map(c => (c.pos, c)).toMap.seq
    // compute new chunk map
    val newChunks: Map[V3I, Chunk] = chunks ++ updated
    // compute which chunks were added
    val added: Set[V3I] = updated.keySet.filterNot(chunks.contains)
    // compute which futures were removed
    val newFutures = futures -- added
    // compute the new border
    val borderGrouped: Map[Boolean, Set[V3I]] =
      (border ++ added).groupBy(dependencies(_).forall(newChunks.contains))
    val newBorder: Set[V3I] = borderGrouped.getOrElse(false, Set.empty)
    val updatedNotBorder: Set[V3I] = updated.keySet ++ borderGrouped.getOrElse(true, Set.empty)
    // compute the new active set
    val byActivity = updatedNotBorder.toSeq.groupBy(newChunks(_).entities.nonEmpty)
    val newActive = active -- byActivity.getOrElse(false, Seq.empty) ++ byActivity.getOrElse(true, Seq.empty)
    // compute the new meshable set
    val byMeshability = updatedNotBorder.toSeq.groupBy(newChunks(_).isMeshable(newChunks.get))
    val newMeshable = meshable -- byMeshability.getOrElse(false, Seq.empty) ++ byMeshability.getOrElse(true, Seq.empty)
    // construct
    new LazyInfWorld(save, time, newChunks, newFutures, newBorder, newActive, newMeshable, entityPosHints)
  }

  def incrTime: LazyInfWorld =
    new LazyInfWorld(save, time + 1, chunks, futures, border, active, meshable, entityPosHints)

  def update(dt: Float): LazyInfWorld = {
    val loadified: LazyInfWorld = loadify
    val events = loadified.active.toSeq.par.map(loadified.chunks(_)).flatMap(_.update(loadified, dt)).seq
    loadified.integrate(events).incrTime
  }

  def renderables(resources: ResourcePack): Seq[RenderUnit] = {
    renderable.flatMap(_.renderables(resources, this))
  }


}
