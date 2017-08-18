package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.{ChunkEvent, World}
import com.phoenixkahlo.hellcraft.graphics.{RenderUnit, ResourcePack}

/**
  * A unit of graphics, logic, or both.
  */
trait Entity {

  def id: UUID

  def update(world: World, ids: Stream[UUID], dt: Float): Seq[ChunkEvent] = Nil

  def renderables(texturePack: ResourcePack): Seq[RenderUnit] = Nil

}
