package com.phoenixkahlo.hellcraft.singleplayer

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{GL20, PerspectiveCamera, Pixmap}
import com.badlogic.gdx.graphics.g3d.shaders.DefaultShader
import com.badlogic.gdx.graphics.g3d._
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.utils.{DefaultShaderProvider, ShaderProvider}
import com.badlogic.gdx.graphics.glutils.FrameBuffer
import com.badlogic.gdx.utils.Disposable
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.graphics.shaders._
import com.phoenixkahlo.hellcraft.math.V3F

import scala.collection.JavaConverters

class Renderer(resources: ResourcePack) extends Disposable {

  val environment = new Environment
  environment.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
  environment.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))

  val cam = new PerspectiveCamera(70, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
  cam.near = 0.1f
  cam.far = 1000
  cam.position.set(V3F(-10, 10, -10) toGdx)
  cam.lookAt(0, 10, 0)

  val sceneShader = new SceneShader(resources.sheet)
  sceneShader.init()
  val lineShader = new LineShader
  lineShader.init()

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

  //val lightBuffer = new FrameBuffer(GL20.GL_DEPTH_COMPONENT16, 1024, 1024, true)


  def render(providers: Seq[RenderableProvider]): Unit = {
    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)

    batch.begin(cam)
    batch.render(JavaConverters.asJavaIterable(providers), environment)
    batch.end()
  }

  override def dispose(): Unit = {
    sceneShader.dispose()
    lineShader.dispose()
  }

}
