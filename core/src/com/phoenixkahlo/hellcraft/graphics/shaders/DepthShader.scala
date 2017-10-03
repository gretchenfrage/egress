package com.phoenixkahlo.hellcraft.graphics.shaders


import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Camera, GL20}
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.graphics.g3d.{Renderable, Shader, utils}
import com.badlogic.gdx.graphics.glutils.GeomShaderProgram.ShaderPart
import com.badlogic.gdx.graphics.glutils.{GeomShaderProgram, ShaderProgram, ShaderStage}
import com.badlogic.gdx.utils.GdxRuntimeException

class DepthShader extends Shader {

  var program: GeomShaderProgram = _
  var cam: Camera = _
  var context: RenderContext = _

  var u_combinedTrans: Int = _

  override def canRender(instance: Renderable): Boolean = true

  override def init(): Unit = {
    val vert = Gdx.files.internal("shaders/depth_v.glsl").readString()
    val geom = Gdx.files.internal("shaders/depth_g.glsl").readString()
    val frag = Gdx.files.internal("shaders/depth_f.glsl").readString()
    //program = new ShaderProgram(vert, frag)
    program = new GeomShaderProgram(
      new ShaderPart(ShaderStage.vertex, vert),
      new ShaderPart(ShaderStage.geometry, geom),
      new ShaderPart(ShaderStage.fragment, frag)
    )

    if (!(program isCompiled))
      throw new GdxRuntimeException(program.getLog)

    u_combinedTrans = program.getUniformLocation("u_combinedTrans")
  }

  override def compareTo(other: Shader): Int = 0

  override def begin(camera: Camera, context: utils.RenderContext): Unit = {
    this.cam = camera
    this.context = context

    program.begin()

    context.setDepthTest(GL20.GL_LEQUAL)
    context.setCullFace(GL20.GL_BACK)
  }

  override def render(renderable: Renderable): Unit = {
    program.setUniformMatrix(u_combinedTrans, renderable.worldTransform.cpy.mulLeft(cam.combined))
    renderable.meshPart.render(program)
  }

  override def end(): Unit = {
    program.end()
  }

  override def dispose(): Unit = {
    program.dispose()
  }
}
