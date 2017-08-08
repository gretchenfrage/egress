package com.phoenixkahlo.hellcraft.infinitetest

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.graphics.{RenderableFactory, ResourceNode, ResourcePack}
import com.phoenixkahlo.hellcraft.math.{Directions, V3I}
import com.phoenixkahlo.hellcraft.save.WorldSave

import scala.collection.immutable.TreeSet
import scala.collection.mutable.ArrayBuffer

class InfinityManager(save: WorldSave, generator: V3I => Block) {

  val buffer = new SaveBuffer(save, generator)
  @volatile var world = HashCacheWorld(0)

  def findEntity(id: UUID): Entity = world.findEntity(id).get

  def makeLoaded(ps: Seq[V3I]): Unit = {
    val needToLoad = ps filterNot (world chunkIsDefinedAt)
    if (needToLoad isEmpty) return
    world ++= buffer.pull(needToLoad.toSeq).values.toSeq
  }

  def makeUnloaded(ps: Seq[V3I]): Unit = {
    val needToUnload = ps filter (world chunkIsDefinedAt) map (world chunkAt _ get)
    if (needToUnload isEmpty) return
    buffer.push(needToUnload.toSeq)
    world --= ps.toSeq
  }

  def setLoaded(ps: Seq[V3I]): Unit = {
    makeLoaded(ps.filterNot(world.loaded.contains))
    val psSet = ps.toSet
    makeUnloaded(world.loaded.values.map(_.pos).filterNot(psSet.contains).toSeq)
  }

  def update(ps: Seq[V3I], updateBuffer: Boolean): Unit = {
    val events = ps.flatMap(world.chunkAt(_)).flatMap(_.update(world))
    integrate(events)
    if (updateBuffer)
      buffer.update(world.loaded.keys.toSeq, 3, world)
  }

  def integrate(events: Seq[ChunkEvent]): Unit = {
    world = world.integrate(events.foldLeft(new TreeSet[ChunkEvent])(_ + _)).incrTime
  }

  def transformChunk(p: V3I, f: Chunk => Chunk): Unit = {
    world = world.transformChunk(p, f)
  }

  def renderables(textures: ResourcePack, ps: Seq[V3I]): Seq[RenderableFactory] = {
    ps.map(world.chunkAt).filter(_ isDefined).map(_.get).flatMap(_.renderables(textures, world))
    //erode(world.loaded.keys.toSet, 1).toSeq.map(world.chunkAt).map(_.get).flatMap(_.renderables(textures, world))
  }

  def resources(textures: ResourcePack): Seq[ResourceNode] = {
    world.loaded.values.flatMap(_.renderables(textures, world)).flatMap(_.resources).toSeq ++
      buffer.resources(textures, world)
  }

  def close(): Unit = {
    buffer.push(world.loaded.values.toSeq)
    buffer.close(world)
  }

}
