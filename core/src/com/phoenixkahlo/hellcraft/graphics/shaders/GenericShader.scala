package com.phoenixkahlo.hellcraft.graphics.shaders

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.graphics.g3d.{Renderable, Shader}
import com.badlogic.gdx.graphics.glutils.GeomShaderProgram.ShaderPart
import com.badlogic.gdx.graphics.glutils.{GeomShaderProgram, ShaderProgram, ShaderStage}
import com.badlogic.gdx.graphics.{Camera, GL20, PerspectiveCamera, Texture}
import com.badlogic.gdx.utils.GdxRuntimeException
import com.phoenixkahlo.hellcraft.math.{Origin, V3F}

class GenericShader(sheet: Texture) extends Shader {


  var program: GeomShaderProgram = _
  var cam: Camera = _
  var context: RenderContext = _

  var u_worldTrans: Int = _
  var u_viewTrans: Int = _
  var u_projTrans: Int = _
  var u_lightPos: Int = _
  var u_texture: Int = _
  var u_lightPow: Int = _

  var lightPow: Float = 1
  var lightPos: V3F = Origin

  override def canRender(instance: Renderable): Boolean = {
    instance.userData == GenericSID
  }

  override def init(): Unit = {
    val vert = Gdx.files.internal("shaders/generic_v.glsl").readString()
    val geom = Gdx.files.internal("shaders/generic_g.glsl").readString()
    val frag = Gdx.files.internal("shaders/generic_f.glsl").readString()
    program = new GeomShaderProgram(
      new ShaderPart(ShaderStage.vertex, vert),
      new ShaderPart(ShaderStage.geometry, geom),
      new ShaderPart(ShaderStage.fragment, frag)
    )

    if (!(program isCompiled))
      throw new GdxRuntimeException(program.getLog)

    u_worldTrans = program.getUniformLocation("u_worldTrans")
    u_viewTrans = program.getUniformLocation("u_viewTrans")
    u_projTrans = program.getUniformLocation("u_projTrans")
    u_lightPos = program.getUniformLocation("u_lightPos")
    u_texture = program.getUniformLocation("u_texture")
    u_lightPow = program.getUniformLocation("u_lightPow")
  }

  override def compareTo(other: Shader): Int = 0

  override def begin(camera: Camera, context: RenderContext): Unit = {
    this.cam = camera
    this.context = context

    program.begin()

    program.setUniformMatrix(u_viewTrans, cam.view)
    program.setUniformf(u_lightPow, lightPow)
    program.setUniformMatrix(u_projTrans, cam.projection)
    program.setUniform3fv(u_lightPos, Array(lightPos.x, lightPos.y, lightPos.z), 0, 3)
    sheet.bind(0)
    program.setUniformi(u_texture, 0)

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
