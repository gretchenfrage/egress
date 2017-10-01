package com.phoenixkahlo.hellcraft.graphics.shaders

import com.badlogic.gdx.graphics.Camera
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.{Gdx, graphics}
import com.badlogic.gdx.graphics.g3d.{Renderable, Shader, utils}
import com.badlogic.gdx.graphics.glutils.GeomShaderProgram.ShaderPart
import com.badlogic.gdx.graphics.glutils.{GeomShaderProgram, ShaderStage}
import com.badlogic.gdx.utils.GdxRuntimeException

class PointShader extends Shader {

  var program: GeomShaderProgram = _
  var cam: Camera = _
  var context: RenderContext = _

  var u_projViewTrans: Int = _
  var u_worldTrans: Int = _

  override def init(): Unit = {
    val vert = Gdx.files.internal("shaders/point_v.glsl").readString()
    val geom = Gdx.files.internal("shaders/point_g.glsl").readString()
    val frag = Gdx.files.internal("shaders/point_f.glsl").readString()
    program = new GeomShaderProgram(
      new ShaderPart(ShaderStage.vertex, vert),
      new ShaderPart(ShaderStage.geometry, geom),
      new ShaderPart(ShaderStage.fragment, frag)
    )

    if (!(program isCompiled))
      throw new GdxRuntimeException(program.getLog)

    u_projViewTrans = program.getUniformLocation("u_projViewTrans")
    u_worldTrans = program.getUniformLocation("u_worldTrans")
  }

  override def begin(camera: Camera, context: RenderContext): Unit = {
    this.cam = camera
    this.context = context
    program.begin()
    program.setUniformMatrix(u_projViewTrans, camera.combined)
  }

  override def render(renderable: Renderable): Unit = {
    program.setUniformMatrix(u_worldTrans, renderable.worldTransform)
    renderable.meshPart.render(program)
  }

  override def end(): Unit = {
    program.end()
  }

  override def dispose(): Unit = {
    program.dispose()
  }

  override def compareTo(other: Shader): Int = 0

  override def canRender(instance: Renderable): Boolean = instance.userData == PointSID

}
