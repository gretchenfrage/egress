package com.phoenixkahlo.hellcraft.fgraphics.procedures

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Camera, Mesh}
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.phoenixkahlo.hellcraft.fgraphics.{GlobalRenderData, HUDShader, ShaderProcedure, ShaderTag}
import com.phoenixkahlo.hellcraft.graphics.HUDComponent

import scala.reflect.ClassTag

class HUDShaderProcedure extends ShaderProcedure[HUDShader] {
  val batch = new SpriteBatch

  override def toFinalForm(comp: HUDComponent): HUDComponent = comp

  override def shader: ShaderTag[HUDShader] = ClassTag(classOf[HUDShader])

  override def isSprites: Boolean = true

  override def begin(globals: GlobalRenderData, context: RenderContext, cam: Camera): Unit = {
    batch.getProjectionMatrix.setToOrtho2D(0, 0, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    batch.begin()
  }

  override def apply(comp: HUDComponent, params: Unit, globals: GlobalRenderData, context: RenderContext, cam: Camera): Unit = {
    comp.draw(batch, cam)
  }

  override def end(): Unit = {
    batch.end()
  }

  override def close(): Unit = {
    batch.dispose()
  }
}
