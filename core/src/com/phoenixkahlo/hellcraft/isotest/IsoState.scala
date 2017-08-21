package com.phoenixkahlo.hellcraft.isotest

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{GL20, PerspectiveCamera}
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.{Environment, ModelBatch, Renderable, RenderableProvider}
import com.badlogic.gdx.graphics.g3d.utils.FirstPersonCameraController
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState}
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.graphics.`new`.NoInterpolation
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}
import com.phoenixkahlo.hellcraft.util.caches.Cache
import com.phoenixkahlo.hellcraft.util.fields.ByteField
import other.OpenSimplexNoise

class IsoState(providedResources: Cache[ResourcePack]) extends GameState {

  private var world: ArrayWorld = _
  private var resources: ResourcePack = _
  private var cam: PerspectiveCamera = _
  private var controller: FirstPersonCameraController = _
  private var batch: ModelBatch = _
  private var lights: Environment = _

  override def onEnter(driver: GameDriver): Unit = {
    val noise = new OpenSimplexNoise

    val gen: V3I => Float = v => {
      val v2 = v / 10
      (noise.eval(v2.x, v2.y, v2.z).toFloat + 1) / 2
    }

    world = new ArrayWorld(gen)


    resources = providedResources()

    cam = new PerspectiveCamera(90, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.1f
    cam.far = 1000
    cam.position.set(V3F(-10, 10, -10) toGdx)
    cam.lookAt(0, 10, 0)

    controller = new FirstPersonCameraController(cam)
    Gdx.input.setInputProcessor(controller)

    batch = new ModelBatch()

    lights = new Environment
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))

    println(world.chunkAt(V3I(3, 3, 3)).get.densities)
    println(world.chunkAt(V3I(3, 3, 3)).get.terrain.vertices(world))
    println(world.chunkAt(V3I(3, 3, 3)).get.terrain.quads(world))
  }

  override def render(): Unit = {
    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)

    controller.update(Gdx.graphics.getDeltaTime)

    val factories = world.renderables(resources)
    val provider = new RenderableProvider {
      override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit =
        factories.flatMap(_(NoInterpolation)).foreach(renderables.add)
    }
    batch.begin(cam)
    batch.render(provider, lights)
    batch.end()
  }

  override def onExit(): Unit = ()

}
