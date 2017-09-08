package com.phoenixkahlo.hellcraft.graphics.shaders

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Camera, GL20, Texture}
import com.badlogic.gdx.graphics.g3d.{Renderable, Shader}
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.graphics.glutils.ShaderProgram
import com.badlogic.gdx.utils.GdxRuntimeException

class BasicShader(sheet: Texture, shaderID: ShaderID) extends Shader {

  var program: ShaderProgram = _
  var cam: Camera = _
  var context: RenderContext = _

  var u_MVP: Int = _
  var u_texture: Int = _

  override def canRender(instance: Renderable): Boolean = {
    instance.userData == shaderID
  }

  override def init(): Unit = {
    val vert = Gdx.files.internal("shaders/basic_v.glsl").readString()
    val frag = Gdx.files.internal("shaders/basic_f.glsl").readString()
    program = new ShaderProgram(vert, frag)

    if (!(program isCompiled))
      throw new GdxRuntimeException(program.getLog)

    u_MVP = program.getUniformLocation("u_MVP")
    u_texture = program.getUniformLocation("u_texture")
  }

  override def compareTo(other: Shader): Int = 0

  override def begin(camera: Camera, context: RenderContext): Unit = {
    this.cam = camera
    this.context = context

    program.begin()

    sheet.bind(1)
    program.setUniformi(u_texture, 1)

    context.setDepthTest(GL20.GL_LEQUAL)
    context.setCullFace(GL20.GL_BACK)
  }

  override def render(renderable: Renderable): Unit = {
    program.setUniformMatrix(u_MVP, renderable.worldTransform.cpy().mulLeft(cam.combined))
    renderable.meshPart.render(program)
  }

  override def end(): Unit = {
    program.end()
  }

  override def dispose(): Unit = {
    program.dispose()
  }
}
