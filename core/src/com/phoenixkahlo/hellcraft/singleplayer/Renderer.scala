package com.phoenixkahlo.hellcraft.singleplayer

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics._
import com.badlogic.gdx.graphics.g3d.shaders.DefaultShader
import com.badlogic.gdx.graphics.g3d._
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.utils.{DefaultShaderProvider, ShaderProvider}
import com.badlogic.gdx.graphics.glutils.FrameBuffer
import com.badlogic.gdx.utils.Disposable
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.graphics.shaders._
import com.phoenixkahlo.hellcraft.math.{Down, V3F, V3I}

import scala.collection.JavaConverters

class Renderer(resources: ResourcePack) extends Disposable {

  val environment = new Environment
  environment.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
  environment.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))

  val cam = new PerspectiveCamera(70, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
  cam.near = 0.1f
  cam.far = 50
  cam.position.set(V3F(-10, 10, -10) toGdx)
  cam.lookAt(0, 10, 0)

  val lightBuffer = new FrameBuffer(Pixmap.Format.RGBA8888, 1024, 1024, true)
  val lightCam = new OrthographicCamera(50, 50)
  lightCam.position.set(0, 10, 0)
  lightCam.direction.set(V3I(1, -1, 1).normalize.toGdx)
  lightCam.up.set(0, 1, 0)
  lightCam.near = 1
  lightCam.far = 50
  lightCam.update()

  val sceneShader = new SceneShader(resources.sheet, lightCam)
  sceneShader.init()
  val lineShader = new LineShader
  lineShader.init()
  val depthShader = new DepthShader
  depthShader.init()

  val batch = new ModelBatch(new ShaderProvider {
    override def getShader(renderable: Renderable): Shader =
      if (renderable.userData == null) {
        if (renderable.shader != null) renderable.shader
        else {
          val shader = new DefaultShader(renderable)
          shader.init()
          renderable.shader = shader
          shader
        }
      } else renderable.userData.asInstanceOf[ShaderID] match {
        case SceneSID => sceneShader
        case LineSID => lineShader
      }

    override def dispose(): Unit = ()
  })

  val depthBatch = new ModelBatch(new ShaderProvider {
    override def getShader(renderable: Renderable): Shader = depthShader

    override def dispose(): Unit = ()
  })


  def render(providers: Seq[RenderableProvider]): Unit = {
    val toRender = JavaConverters.asJavaIterable(providers)

    lightBuffer.begin()

    Gdx.gl.glClearColor(1, 1, 1, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)

    depthBatch.begin(lightCam)
    depthBatch.render(toRender, environment)
    depthBatch.end()

    lightBuffer.end()

    sceneShader.depthMap = lightBuffer.getColorBufferTexture

    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)

    if (!Gdx.input.isKeyPressed(Keys.M)) {
      batch.begin(cam)
      batch.render(JavaConverters.asJavaIterable(providers), environment)
      batch.end()
    } else {
      depthBatch.begin(lightCam)
      depthBatch.render(toRender, environment)
      depthBatch.end()
    }
  }

  override def dispose(): Unit = {
    sceneShader.dispose()
    lineShader.dispose()
  }

}
