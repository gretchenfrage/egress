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

  private var world: FiniteWorld = _
  private var texturePack: TexturePack = _
  private var cam: PerspectiveCamera = _
  private var controller: MMovementController = _
  private var modelBatch: ModelBatch = _
  private var lights: Environment = _

  override def create(): Unit = {
    texturePack = new DefaultTexturePack

    world = new FiniteWorld(V3I(15, 5, 15), 16)

    println("generating")

    MathUtils.random.setSeed("phoenix".hashCode)

    val heights = PerlinNoiseGenerator.generateHeightMap((world.size * 16).xi, (world.size * 16).zi, 0, 63, 9)
    def height(v: V3I): Byte = heights(v.zi * world.size.zi * 16 + v.xi)
    world = world.mapBlocks(v => {
      val depth = height(v) - v.y
      if (depth > 20) Stone
      else if (depth >= 0) Dirt
      else Air
    })

    /*
    world = world.mapChunks(c => {
      (Origin until c.pos).foldLeft(c)({ case (cc, v) => cc.updateBlock(v, Stone) })
    })
    */
    /*
    for (v <- Origin until world.size) {
      world = world.updateChunk(
        v,
        // it's the c.pos that breaks it...
        // idea one: chunkAt it broken
        // idea two: chunks don't know where the heck they are
        c => (Origin to c.pos).foldLeft(c)({ case (cc, vv) => cc.updateBlock(vv, Stone) })
      )
    }
    */

    //world = world.mapBlocks(_ => Stone)
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
        world.renderables(texturePack).flatMap(_()).foreach(renderables.add)
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
