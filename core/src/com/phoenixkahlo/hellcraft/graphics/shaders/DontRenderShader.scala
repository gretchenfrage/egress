package com.phoenixkahlo.hellcraft.graphics.shaders

import com.badlogic.gdx.graphics.Camera
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.graphics.g3d.{Renderable, Shader}

object DontRenderShader extends Shader {
  override def canRender(instance: Renderable): Boolean = true

  override def init(): Unit = ()

  override def compareTo(other: Shader): Int = 0

  override def end(): Unit = ()

  override def begin(camera: Camera, context: RenderContext): Unit = ()

  override def render(renderable: Renderable): Unit = ()

  override def dispose(): Unit = ()
}
