package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.graphics.g3d.Renderable

/**
  * Produces a renderable. This object should only be invoked when a
  * LibGDX/OpenGL context is set up.
  */
trait RenderableFactory {

  def apply(): Renderable

}
