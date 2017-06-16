package com.phoenixkahlo.hellcraft

import java.util.UUID

import com.badlogic.gdx.graphics.g3d.{Renderable, RenderableProvider}

/**
  * A unit of graphics, logic, or both.
  */
trait Entity {

  def id: UUID

  def update(world: World): Seq[ChunkEvent] = Nil

  def renderables(texturePack: TexturePack): Seq[RenderableFactory] = Nil

}
