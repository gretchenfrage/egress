package com.phoenixkahlo.hellcraft.graphics.`new`

import javax.print.attribute.AttributeSet

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.g3d.attributes.TextureAttribute
import com.badlogic.gdx.graphics.{Camera, GL20, PerspectiveCamera, Texture}
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.graphics.g3d.{Attribute, Attributes, Renderable, Shader}
import com.badlogic.gdx.graphics.glutils.ShaderProgram
import com.badlogic.gdx.utils.GdxRuntimeException
import com.phoenixkahlo.hellcraft.graphics.ResourcePack

class TestShader(sheet: Texture, light: PerspectiveCamera) extends Shader {

  var program: ShaderProgram = _
  private var cam: Camera = _
  private var context: RenderContext = _

  /*
  uniform sampler2D u_texture;
uniform samplerCube u_depthMap;
uniform float u_cameraFar;
uniform vec3 u_lightPosition;
   */

  private var u_projViewTrans: Int = _
  private var u_worldTrans: Int = _
  private var u_lightTrans: Int = _
  private var u_texture: Int = _
  private var u_depthMap: Int = _
  private var u_cameraFar: Int = _
  private var u_lightPosition: Int = _
  var depthMap: Texture = _

  override def canRender(instance: Renderable): Boolean = {
    instance.userData == CustomSID
  }

  override def init(): Unit = {
    val vert = Gdx.files.internal("shaders/test_v.glsl").readString()
    val frag = Gdx.files.internal("shaders/test_f.glsl").readString()
    program = new ShaderProgram(vert, frag)

    if (!(program isCompiled))
      throw new GdxRuntimeException(program.getLog)

    u_projViewTrans = program.getUniformLocation("u_projViewTrans")
    u_worldTrans = program.getUniformLocation("u_worldTrans")
    u_lightTrans = program.getUniformLocation("u_lightTrans")
    u_texture = program.getUniformLocation("u_texture")
    u_depthMap = program.getUniformLocation("u_depthMap")
    u_cameraFar = program.getUniformLocation("u_cameraFar")
    u_lightPosition = program.getUniformLocation("u_lightPosition")
  }

  override def compareTo(other: Shader): Int = 0

  override def begin(camera: Camera, context: RenderContext): Unit = {
    this.cam = camera
    this.context = context
    program.begin()

    program.setUniformMatrix(u_projViewTrans, camera.combined)
    program.setUniformMatrix(u_lightTrans, light.combined)
    sheet.bind()
    program.setUniformi(u_texture, 0)
    program.setUniformf(u_cameraFar, light.far)
    program.setUniform3fv(u_lightPosition, Array(light.position.x, light.position.y, light.position.z), 0, 3)
    depthMap.bind(2)
    program.setUniformi(u_depthMap, 2)

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
