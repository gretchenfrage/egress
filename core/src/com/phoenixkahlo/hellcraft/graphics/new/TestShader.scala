package com.phoenixkahlo.hellcraft.graphics.`new`

import javax.print.attribute.AttributeSet

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.g3d.attributes.TextureAttribute
import com.badlogic.gdx.graphics.{Camera, GL20, Texture}
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.graphics.g3d.{Attribute, Attributes, Renderable, Shader}
import com.badlogic.gdx.graphics.glutils.ShaderProgram
import com.badlogic.gdx.utils.GdxRuntimeException
import com.phoenixkahlo.hellcraft.graphics.ResourcePack

class TestShader(sheet: Texture) extends Shader {

  var program: ShaderProgram = _
  var cam: Camera = _
  var context: RenderContext = _
  var u_projViewTrans: Int = _
  var u_worldTrans: Int = _

  override def canRender(instance: Renderable): Boolean = {
    instance.meshPart.primitiveType == GL20.GL_TRIANGLES
  }

  override def init(): Unit = {
    val vert = Gdx.files.internal("shaders/test_v.glsl").readString()
    val frag = Gdx.files.internal("shaders/test_f.glsl").readString()
    program = new ShaderProgram(vert, frag)
    if (!(program isCompiled))
      throw new GdxRuntimeException(program.getLog)
    u_projViewTrans = program.getUniformLocation("u_projViewTrans")
    u_worldTrans = program.getUniformLocation("u_worldTrans")
  }

  override def compareTo(other: Shader): Int = 0

  override def begin(camera: Camera, context: RenderContext): Unit = {
    this.cam = camera
    this.context = context
    program.begin()
    program.setUniformMatrix(u_projViewTrans, camera.combined)
    sheet.bind()
    program.setUniformi("u_texture", 0)
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
