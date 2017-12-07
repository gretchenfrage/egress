package com.phoenixkahlo.hellcraft.fgraphics.procedures

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.Mesh
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.phoenixkahlo.hellcraft.fgraphics.{GlobalRenderData, HUDShader, ShaderProcedure}
import com.phoenixkahlo.hellcraft.graphics.HUDComponent

class HUDShaderProcedure extends ShaderProcedure[HUDShader] {
  val batch = new SpriteBatch

  override def toFinalForm(comp: HUDComponent): HUDComponent = comp

  override def begin(globals: GlobalRenderData, context: RenderContext): Unit = {
    batch.getProjectionMatrix.setToOrtho2D(0, 0, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    batch.begin()
  }

  override def apply(comp: HUDComponent, params: Unit, globals: GlobalRenderData, context: RenderContext): Unit = {
    comp.draw(batch, globals.cam)
  }

  override def end(): Unit = {
    batch.end()
  }

  override def close(): Unit = {
    batch.dispose()
  }
}
