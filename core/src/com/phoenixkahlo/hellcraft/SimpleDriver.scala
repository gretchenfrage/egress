package com.phoenixkahlo.hellcraft


import com.badlogic.gdx.graphics.{GL20, PerspectiveCamera}
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.{ApplicationAdapter, Gdx}
import com.badlogic.gdx.graphics.g3d.{Environment, ModelBatch, Renderable, RenderableProvider}
import com.badlogic.gdx.math.MathUtils
import com.badlogic.gdx.utils.{Array, Pool}
import com.phoenixkahlo.hellcraft.prototype.MMovementController
import com.phoenixkahlo.hellcraft.util.{Origin, Repeated, V3F, V3I}
import other.PerlinNoiseGenerator

class SimpleDriver extends ApplicationAdapter {

  private var texturePack: TexturePack = _
  private var chunks: Seq[Chunk] = _
  private var cam: PerspectiveCamera = _
  private var controller: MMovementController = _
  private var modelBatch: ModelBatch = _
  private var lights: Environment = _

  override def create(): Unit = {
    texturePack = new DefaultTexturePack

    val size = V3I(4, 4, 4)
    chunks = (Origin until size).map(v => new Chunk(v, 16))

    println("generating")

    MathUtils.random.setSeed("phoenix".hashCode)
    val heights = PerlinNoiseGenerator.generateHeightMap((size * 16).xi, (size * 16).zi, 0, 63, 11)
    def height(v: V3I): Byte = heights.apply(v.zi * size.zi * 16 + v.xi)
    chunks = chunks.map(c => c.mapBlocks(v => {
      val wv = v + (c.pos * 16)
      val depth = (height(wv) - wv.y)
      if (depth > 20) Stone
      else if (depth >= 0) Dirt
      else Air
    }))

    //chunks = chunks.map(_.mapBlocks(_ => Stone))
    println("generated")

    cam = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.1f
    cam.far = 1000
    cam.position.set(V3F(-10, 10, -10) toGdx)
    cam.lookAt(0, 10, 0)

    controller = new MMovementController(cam)
    Gdx.input.setInputProcessor(controller)

    modelBatch = new ModelBatch

    lights = new Environment
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))
  }

  override def render(): Unit = {
    val provider = new RenderableProvider {
      override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit =
        chunks.flatMap(_.renderables(texturePack)).map(_()).foreach(renderables.add)
    }

    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)

    modelBatch.begin(cam)
    modelBatch.render(provider, lights)
    modelBatch.end()

    controller.update()
  }
}
