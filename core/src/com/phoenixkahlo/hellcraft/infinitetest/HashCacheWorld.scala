package com.phoenixkahlo.hellcraft.infinitetest

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.finitetest.FiniteWorld
import com.phoenixkahlo.hellcraft.math.V3I

import scala.collection.Map
import scala.collection.immutable.HashMap

case class HashCacheWorld(loaded: Map[V3I,Chunk] = new HashMap) extends World {

  val cache = new ThreadLocal[Chunk]

  override def chunkAt(chunkPos: V3I): Option[Chunk] = {
    if (cache.get() != null && cache.get().pos == chunkPos) Some(cache.get())
    else {
      val chunk = loaded get chunkPos
      if (chunk isDefined) cache.set(chunk.get)
      chunk
    }
  }

  override def chunkIsDefinedAt(chunkPos: V3I): Boolean = loaded contains chunkPos

  // TODO: global entity positioning system
  override def findEntity(id: UUID): Entity = loaded.values.flatMap(_.entities).toMap.apply(id)

  def renderables(texturePack: TexturePack): Seq[RenderableFactory] =
    loaded.values.flatMap(_.renderables(texturePack, this)).toSeq

  def +(chunk: Chunk): HashCacheWorld = HashCacheWorld(loaded.updated(chunk.pos, chunk))

  def -(p: V3I): HashCacheWorld = HashCacheWorld(loaded - p)

  def ++(chunks: Seq[Chunk]): HashCacheWorld =
    chunks.foldLeft(this)({ case (world, chunk) => world.+(chunk) })

  def --(ps: Seq[V3I]): HashCacheWorld =
    ps.foldLeft(this)({ case (world, p) => world.-(p) })

  def mapChunks(f: Chunk => Chunk): HashCacheWorld =
    HashCacheWorld(loaded.par.mapValues(f).seq)

  def integrate(events: Seq[ChunkEvent]): HashCacheWorld = {
    val grouped = events.groupBy(_.chunkPos)
    mapChunks(c => {
      grouped.get(c.pos) match {
        case Some(e) => e.foldLeft(c)({ case (cc, ee) => ee(cc) })
        case None => c
      }
    })
  }

  def update: HashCacheWorld =
    integrate(loaded.values.flatMap(_.update(this)).toSeq)

  def transformChunk(p: V3I, f: Chunk => Chunk): HashCacheWorld =
    HashCacheWorld(loaded.updated(p, f(loaded(p))))

}
