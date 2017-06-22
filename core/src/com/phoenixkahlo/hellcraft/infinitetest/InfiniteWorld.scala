package com.phoenixkahlo.hellcraft.infinitetest

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.save.WorldSave

class InfiniteWorld(
                   val save: WorldSave,
                   val generator: V3I => Block
                   ) extends World {

  var loaded = HashCacheWorld()

  override def chunkAt(p: V3I): Option[Chunk] = {
    makeLoaded(Seq(p))
    Some(loaded.chunkAt(p).get)
  }

  override def chunkIsDefinedAt(chunkPos: V3I): Boolean = true

  override def findEntity(id: UUID): Entity = loaded.findEntity(id)

  def makeLoaded(ps: Seq[V3I]): Unit = {
    val needToLoad = ps filter (loaded chunkAt _ isEmpty)
    if (needToLoad isEmpty) return
    loaded ++= save.load(needToLoad).values.toSeq
    loaded ++= needToLoad filterNot (loaded chunkIsDefinedAt) map (p => new Chunk(p).mapBlocks(v => generator(p * 16 + v)))
  }

  def makeUnloaded(ps: Seq[V3I]): Unit = {
    save.save(ps map (loaded.chunkAt(_).get), this)
    loaded --= ps
  }

  def setLoaded(ps: Seq[V3I]): Unit = {
    val toLoad = ps.filterNot(loaded.loaded.contains)
    val toUnload = loaded.loaded.values.map(_.pos).filterNot(ps.contains).toSeq
    makeLoaded(toLoad)
    makeUnloaded(toUnload)
  }

  def saveAll(): Unit = {
    save.save(loaded.loaded.values.toSeq, this)
  }

  def update(): Unit = {
    loaded = loaded.update(world = this)
  }

  def integrate(events: Seq[ChunkEvent]): Unit = {
    loaded = loaded.integrate(events)
  }

  def transformChunk(p: V3I, f: Chunk => Chunk): Unit = {
    loaded = loaded.transformChunk(p, f)
  }

  def renderables(textures: TexturePack): Seq[RenderableFactory] = {
    loaded.renderables(textures)
  }

}


//TODO: don't simulate chunk hat aren't completely surrounded by loaded chunks
//TODO: actual intelligent clustering
//TODO: don't necessarily render all loaded chunk
/*
abstract class InfiniteWorld(val save: WorldSave) extends World {

  var loaded = HashCacheWorld()

  protected def generate(v: V3I): Block

  override def chunkAt(p: V3I): Option[Chunk] = {
    // ensure the chunk is loaded
    load(Seq(p))
    // return the chunk
    Some(loaded.chunkAt(p).get)
  }


  override def chunkIsDefinedAt(chunkPos: V3I): Boolean = true

  override def findEntity(id: UUID): Entity = loaded.findEntity(id)

  def load(ps: Seq[V3I]): Unit = {
    val toLoad = ps.filter(loaded.chunkAt(_).isEmpty)
    if (toLoad isEmpty) return
    // load as many values as possible from the save
    loaded ++= save.load(toLoad).values.toSeq
    // generate any chunks that still haven't been loaded
    loaded ++= toLoad.filter(loaded.chunkAt(_).isEmpty).map(p => new Chunk(p).mapBlocks(v => generate((p * 16) + v)))
  }

  def unload(ps: Seq[V3I]): Unit = {
    // save them to the save file
    save.save(ps.map(loaded.chunkAt(_).get), this)
    // unload them from memory
    loaded --= ps
  }

  def update(): Unit = {
    // update the loaded chunks and collect the unintegrated events
    val (updated, unintegrated) = loaded.update(world = this)
    loaded = updated
    // apply the unintegrated events to their chunks by temporarily loading clusters of them
    val grouped = unintegrated.groupBy(_.chunkPos)
    val clusters = grouped.keys.grouped(16)
    for (cluster <- clusters) {
      load(cluster.toSeq)
      loaded = loaded.integrate(cluster.flatMap(grouped.get).flatten.toSeq)._1
      unload(cluster.toSeq)
    }
  }

  def transformChunk(p: V3I, f: Chunk => Chunk): Unit = {
    load(Seq(p))
    loaded = loaded.transformChunk(p, f)
  }

  def renderables(textures: TexturePack): Seq[RenderableFactory] =
    loaded.loaded.values.flatMap(_.renderables(textures, this)).toSeq

}
*/