package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.{ChunkEvent, UpdateEffect, World}
import com.phoenixkahlo.hellcraft.graphics.{RenderUnit, ResourcePack}
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}

trait Entity extends Serializable {
  def id: UUID

  def update(world: World, ids: Stream[UUID]): Seq[UpdateEffect] = Seq.empty

  def renderables(pack: ResourcePack): Seq[RenderUnit] = Seq.empty

  def pos: V3F

  def chunkPos: V3I = pos / 16 floor
}

trait Moveable extends Entity {
  def updatePos(newPos: V3F): Entity
}