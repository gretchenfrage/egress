package com.phoenixkahlo.hellcraft.oldcore.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.oldcore.{ChunkEvent, World}
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.graphics.old.RenderableFactory

/**
  * A unit of graphics, logic, or both.
  */
trait Entity {

  def id: UUID

  def update(world: World, ids: Stream[UUID], dt: Float): Seq[ChunkEvent] = Nil

  def renderables(texturePack: ResourcePack): Seq[RenderableFactory] = Nil

}
