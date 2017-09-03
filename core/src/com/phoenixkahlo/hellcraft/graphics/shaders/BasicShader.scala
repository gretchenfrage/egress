package com.phoenixkahlo.hellcraft.graphics.shaders


import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Camera, GL20, Texture}
import com.badlogic.gdx.graphics.g3d.{Renderable, Shader}
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.graphics.glutils.ShaderProgram
import com.badlogic.gdx.utils.GdxRuntimeException

class BasicShader(texture: Texture, sid: ShaderID) extends Shader {

  var program: ShaderProgram = _
  var cam: Camera = _
  var context: RenderContext = _

  var u_worldTrans: Int = _
  var u_projViewTrans: Int = _
  var u_texture: Int = _

  override def init(): Unit = {
    val vert = Gdx.files.internal("shaders/basic_v.glsl").readString()
    val frag = Gdx.files.internal("shaders/basic_f.glsl").readString()
    program = new ShaderProgram(vert, frag)

    if (!(program isCompiled))
      throw new GdxRuntimeException(program.getLog)

    u_worldTrans = program.getUniformLocation("u_worldTrans")
    u_projViewTrans = program.getUniformLocation("u_projViewTrans")
    u_texture = program.getUniformLocation("u_texture")
  }

  override def begin(camera: Camera, context: RenderContext): Unit = {
    this.cam = camera
    this.context = context

    program.begin()

    program.setUniformMatrix(u_projViewTrans, camera.combined)
    texture.bind(0)
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

  override def compareTo(other: Shader): Int = 0

  override def canRender(instance: Renderable): Boolean = instance.userData == sid

  override def dispose(): Unit = {
    program.dispose()
  }

}
