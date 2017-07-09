package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.{ChunkEvent, RenderableFactory, TexturePack, World}

/**
  * A unit of graphics, logic, or both.
  */
trait Entity {

  def id: UUID

  def update(world: World, ids: Stream[UUID]): Seq[ChunkEvent] = Nil

  def renderables(texturePack: TexturePack): Seq[RenderableFactory] = Nil

}
