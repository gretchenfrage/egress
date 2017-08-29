package com.phoenixkahlo.hellcraft.graphics.`new`

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Camera, GL20}
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.graphics.g3d.{Renderable, Shader}
import com.badlogic.gdx.graphics.glutils.ShaderProgram
import com.badlogic.gdx.utils.GdxRuntimeException

class DepthShader extends Shader {

  private var program: ShaderProgram = _
  private var cam: Camera = _
  private var context: RenderContext = _

  private var u_projViewTrans: Int = _
  private var u_worldTrans: Int = _
  private var u_cameraFar: Int = _
  private var u_lightPosition: Int = _

  override def canRender(instance: Renderable): Boolean = true

  override def init(): Unit = {
    val vert = Gdx.files.internal("shaders/depth_v.glsl").readString()
    val frag = Gdx.files.internal("shaders/depth_f.glsl").readString()
    program = new ShaderProgram(vert, frag)

    if (!(program isCompiled))
      throw new GdxRuntimeException(program.getLog)

    u_projViewTrans = program.getUniformLocation("u_projViewTrans")
    u_worldTrans = program.getUniformLocation("u_worldTrans")
    u_cameraFar = program.getUniformLocation("u_cameraFar")
    u_lightPosition = program.getUniformLocation("u_lightPosition")
  }

  override def compareTo(other: Shader): Int = 0

  override def begin(camera: Camera, context: RenderContext): Unit = {
    this.cam = camera
    this.context = context
    program.begin()
    program.setUniformMatrix(u_projViewTrans, camera.combined)
    program.setUniformf(u_cameraFar, camera.far)
    program.setUniform3fv(u_lightPosition, Array(camera.position.x, camera.position.y, camera.position.z), 0, 3)
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
