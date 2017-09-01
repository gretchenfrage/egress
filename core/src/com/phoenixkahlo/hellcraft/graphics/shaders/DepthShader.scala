package com.phoenixkahlo.hellcraft.graphics.shaders


import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Camera, GL20}
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.graphics.g3d.{Renderable, Shader, utils}
import com.badlogic.gdx.graphics.glutils.ShaderProgram
import com.badlogic.gdx.utils.GdxRuntimeException

class DepthShader extends Shader {

  var program: ShaderProgram = _
  var cam: Camera = _
  var context: RenderContext = _

  var u_worldTrans: Int = _
  var u_projViewTrans: Int = _
  var u_lightFar: Int = _
  var u_lightPos: Int = _

  override def canRender(instance: Renderable): Boolean = true

  override def init(): Unit = {
    val vert = Gdx.files.internal("shaders/depth_v.glsl").readString()
    val frag = Gdx.files.internal("shaders/depth_f.glsl").readString()
    program = new ShaderProgram(vert, frag)

    if (!(program isCompiled))
      throw new GdxRuntimeException(program.getLog)

    u_worldTrans = program.getUniformLocation("u_worldTrans")
    u_projViewTrans = program.getUniformLocation("u_projViewTrans")
    u_lightFar = program.getUniformLocation("u_lightFar")
    u_lightPos = program.getUniformLocation("u_lightPos")
  }

  override def compareTo(other: Shader): Int = 0

  override def begin(camera: Camera, context: utils.RenderContext): Unit = {
    this.cam = camera
    this.context = context

    program.begin()

    program.setUniformMatrix(u_projViewTrans, cam.combined)
    program.setUniformf(u_lightFar, cam.far)
    program.setUniform3fv(u_lightPos, Array(cam.position.x, cam.position.y, cam.position.z), 0, 3)

    context.setDepthTest(GL20.GL_LEQUAL)
    context.setCullFace(GL20.GL_BACK)
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
}
