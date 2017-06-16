package com.phoenixkahlo.hellcraft



import java.util.UUID
import java.util.concurrent.ThreadLocalRandom

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.{GL20, PerspectiveCamera}
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.{ApplicationAdapter, Gdx}
import com.badlogic.gdx.graphics.g3d.{Environment, ModelBatch, Renderable, RenderableProvider}
import com.badlogic.gdx.math.MathUtils
import com.badlogic.gdx.utils.{Array, Pool}
import com.phoenixkahlo.hellcraft.prototype.MMovementController
import com.phoenixkahlo.hellcraft.util._
import other.PerlinNoiseGenerator

import scala.collection.mutable
import scala.util.Random

class SimpleDriver extends ApplicationAdapter {

  private var history: Vector[FiniteWorld] = _
  private var world: FiniteWorld = _
  private var texturePack: TexturePack = _
  private var cam: PerspectiveCamera = _
  private var controller: AvatarController = _
  private var modelBatch: ModelBatch = _
  private var lights: Environment = _

  override def create(): Unit = {
    history = Vector()

    texturePack = new DefaultTexturePack

    world = new FiniteWorld(V3I(16, 16, 16), 16)

    println("generating")

    MathUtils.random.setSeed("phoenix".hashCode)
    val heights = PerlinNoiseGenerator.generateHeightMap((world.size * 16).xi, (world.size * 16).zi, 0, 128, 9)
    def height(v: V3I): Byte = heights(v.zi * world.size.zi * 16 + v.xi)
    world = world.mapBlocks(v => {
      val depth = height(v) - v.y
      if (depth > 20) Stone
      else if (depth >= 0)
        if (ThreadLocalRandom.current().nextBoolean()) Dirt
        else Brick
      else Air
    })
    println("generated")

    val avatar = new Avatar()
    world = world.transformChunk(avatar.chunkPos, _.putEntity(avatar))

    cam = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.1f
    cam.far = 1000
    cam.position.set(V3F(-10, 10, -10) toGdx)
    cam.lookAt(0, 10, 0)

    controller = AvatarController(cam, avatar.id)
    Gdx.input.setInputProcessor(controller)

    modelBatch = new ModelBatch

    lights = new Environment
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))
  }

  override def render(): Unit = {
    if (true) {
      world = world.update
      world = world.integrate(controller.update(world))
    } else {
      if (Gdx.input.isKeyPressed(Keys.P)) {
        history = history :+ world
        world = world.update
        world = world.integrate(controller.update(world))
      }
      if (Gdx.input.isKeyPressed(Keys.L) && history.nonEmpty) {
        world = history.last
        history = history.dropRight(1)
      }
      if (history.size > 6000)
        history = history.drop(6000 - history.size)
    }

    controller.postUpdate(world)

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
  }
}
