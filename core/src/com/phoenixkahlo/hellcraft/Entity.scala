package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.graphics.g3d.{Renderable, RenderableProvider}

/**
  * A unit of graphics, logic, or both.
  */
trait Entity {

  def update: Seq[ChunkEvent] = Nil

  def renderables(texturePack: TexturePack): Seq[RenderableFactory] = Nil

}
