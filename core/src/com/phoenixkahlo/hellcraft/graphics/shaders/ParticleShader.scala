package com.phoenixkahlo.hellcraft.graphics.shaders


import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Camera, GL20, Texture}
import com.badlogic.gdx.graphics.g3d.{Renderable, Shader, utils}
import com.badlogic.gdx.graphics.glutils.GeomShaderProgram.ShaderPart
import com.badlogic.gdx.graphics.glutils.{GeomShaderProgram, ShaderStage}
import com.badlogic.gdx.utils.GdxRuntimeException

class ParticleShader(sheet: Texture) extends Shader {

  var program: GeomShaderProgram = _
  var cam: Camera = _
  var context: utils.RenderContext = _

  var u_MVP: Int = _
  var u_texture: Int = _

  override def init(): Unit = {
    val vert = Gdx.files.internal("shaders/particle_v.glsl").readString()
    val geom = Gdx.files.internal("shaders/particle_g.glsl").readString()
    val frag = Gdx.files.internal("shaders/particle_f.glsl").readString()
    program = new GeomShaderProgram(
      new ShaderPart(ShaderStage.vertex, vert),
      new ShaderPart(ShaderStage.geometry, geom),
      new ShaderPart(ShaderStage.fragment, frag)
    )

    if (!(program isCompiled))
      throw new GdxRuntimeException(program.getLog)

    u_MVP = program.getUniformLocation("u_MVP")
    u_texture = program.getUniformLocation("u_texture")
  }

  override def begin(camera: Camera, context: utils.RenderContext): Unit = {
    this.cam = camera
    this.context = context
    program.begin()

    sheet.bind(0)
    program.setUniformi(u_texture, 0)

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

  override def compareTo(other: Shader): Int = 0

  override def canRender(instance: Renderable): Boolean = true
}
