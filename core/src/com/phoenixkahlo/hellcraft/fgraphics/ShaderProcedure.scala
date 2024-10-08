package com.phoenixkahlo.hellcraft.fgraphics

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Camera, GL20, Mesh}
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.graphics.glutils.GeomShaderProgram.ShaderPart
import com.badlogic.gdx.graphics.glutils.{GeomShaderProgram, ShaderStage}
import com.badlogic.gdx.utils.GdxRuntimeException
import com.phoenixkahlo.hellcraft.ShaderTag

trait ShaderProcedure[S <: Shader] {
  def toFinalForm(renderUnit: S#RenderUnit): S#FinalForm
  def begin(globals: GlobalRenderData, context: RenderContext, cam: Camera): Unit
  def isSprites: Boolean = false
  def apply(mesh: S#FinalForm, params: S#Params, globals: GlobalRenderData, context: RenderContext, cam: Camera): Unit
  def end(): Unit
  def close(): Unit
  def shader: ShaderTag[S]
  def disposer: Option[S#FinalForm => Unit]
}

