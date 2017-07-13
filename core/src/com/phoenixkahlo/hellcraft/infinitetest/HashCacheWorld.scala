package com.phoenixkahlo.hellcraft.infinitetest

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.math.V3I

import scala.collection.{Map, SortedSet}
import scala.collection.immutable.{HashMap, TreeSet}

case class HashCacheWorld(time: Long, loaded: Map[V3I, Chunk] = new HashMap) extends World {

  val cache = new ThreadLocal[Chunk]

  override def chunkAt(chunkPos: V3I): Option[Chunk] = {
    val cached = cache.get()
    if (cached != null && cached.pos == chunkPos) Some(cached)
    else {
      val chunk = loaded get chunkPos
      if (chunk isDefined) cache.set(chunk.get)
      chunk
    }
  }

  override def chunkIsDefinedAt(chunkPos: V3I): Boolean = loaded contains chunkPos

  override def findEntity(id: UUID): Option[Entity] = loaded.values.flatMap(_.entities).toMap.get(id)

  def renderables(texturePack: TexturePack): Seq[RenderableFactory] =
    loaded.values.flatMap(_.renderables(texturePack, this)).toSeq

  def +(chunk: Chunk): HashCacheWorld = HashCacheWorld(time, loaded.updated(chunk.pos, chunk))

  def -(p: V3I): HashCacheWorld = HashCacheWorld(time, loaded - p)

  def ++(chunks: Seq[Chunk]): HashCacheWorld =
    chunks.foldLeft(this)({ case (world, chunk) => world.+(chunk) })

  def --(ps: Seq[V3I]): HashCacheWorld =
    ps.foldLeft(this)({ case (world, p) => world.-(p) })

  def integrate(events: SortedSet[ChunkEvent]): HashCacheWorld = {
    copy(loaded = events.groupBy(_.target).par.flatMap({
      case (p, eventGroup) => eventGroup.foldLeft(loaded.get(p))({ case (cc, ee) => cc.map(ee(_)) })
    }).foldLeft(loaded)({ case (map, chunk) => map.updated(chunk.pos, chunk) }))
  }

  def incrTime: HashCacheWorld =
    copy(time = time + 1)

  def update: HashCacheWorld =
    integrate(loaded.values.par.flatMap(_.update(this)).seq.foldLeft(new TreeSet[ChunkEvent])(_ + _)).incrTime

  def transformChunk(p: V3I, f: Chunk => Chunk): HashCacheWorld =
    HashCacheWorld(time, loaded.updated(p, f(loaded(p))))

}
