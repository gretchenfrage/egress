package com.phoenixkahlo.hellcraft.graphics.shaders

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.graphics.g3d.{Renderable, Shader}
import com.badlogic.gdx.graphics.glutils.ShaderProgram
import com.badlogic.gdx.graphics.{Camera, GL20, PerspectiveCamera, Texture}
import com.badlogic.gdx.utils.GdxRuntimeException

class SceneShader(sheet: Texture, light: Camera) extends Shader {


  var program: ShaderProgram = _
  var cam: Camera = _
  var context: RenderContext = _

  var u_worldTrans: Int = _
  var u_viewTrans: Int = _
  var u_projTrans: Int = _
  var u_shadowProjViewTrans: Int = _
  var u_lightPos: Int = _
  var u_texture: Int = _
  var u_depthMap: Int = _

  var depthMap: Texture = _

  override def canRender(instance: Renderable): Boolean = {
    instance.userData == SceneSID
  }

  override def init(): Unit = {
    val vert = Gdx.files.internal("shaders/scene_v.glsl").readString()
    val frag = Gdx.files.internal("shaders/scene_f.glsl").readString()
    program = new ShaderProgram(vert, frag)

    if (!(program isCompiled))
      throw new GdxRuntimeException(program.getLog)

    u_worldTrans = program.getUniformLocation("u_worldTrans")
    u_viewTrans = program.getUniformLocation("u_viewTrans")
    u_projTrans = program.getUniformLocation("u_projTrans")
    u_shadowProjViewTrans = program.getUniformLocation("u_shadowProjViewTrans")
    u_lightPos = program.getUniformLocation("u_lightPos")
    u_texture = program.getUniformLocation("u_texture")
    u_depthMap = program.getUniformLocation("u_depthMap")
  }

  override def compareTo(other: Shader): Int = 0

  override def begin(camera: Camera, context: RenderContext): Unit = {
    this.cam = camera
    this.context = context

    program.begin()

    program.setUniformMatrix(u_viewTrans, cam.view)
    program.setUniformMatrix(u_projTrans, cam.projection)
    program.setUniformMatrix(u_shadowProjViewTrans, light.combined)
    program.setUniform3fv(u_lightPos, Array(light.position.x, light.position.y, light.position.z), 0, 3)
    sheet.bind(0)
    program.setUniformi(u_texture, 0)
    depthMap.bind(1)
    program.setUniformi(u_depthMap, 1)

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
