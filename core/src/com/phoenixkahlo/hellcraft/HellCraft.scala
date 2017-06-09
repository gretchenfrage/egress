package com.phoenixkahlo.hellcraft



import java.util.concurrent.ThreadLocalRandom

import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.utils.FirstPersonCameraController
import com.badlogic.gdx.graphics.g3d.{Environment, ModelBatch}
import com.badlogic.gdx.graphics.{GL20, PerspectiveCamera}
import com.badlogic.gdx.math.{MathUtils, Vector3}
import com.badlogic.gdx.physics.bullet.Bullet
import com.badlogic.gdx.physics.bullet.collision.{btCollisionDispatcher, btDbvtBroadphase, btDefaultCollisionConfiguration}
import com.badlogic.gdx.physics.bullet.dynamics.{btConstraintSolver, btDiscreteDynamicsWorld, btDynamicsWorld, btSequentialImpulseConstraintSolver}
import com.badlogic.gdx.{ApplicationAdapter, Gdx}
import com.phoenixkahlo.hellcraft.util._
import other.PerlinNoiseGenerator

import scala.util.Random

class HellCraft extends ApplicationAdapter {

  private var world: World = _
  private var worldRenderer: WorldRenderer = _
  private var cam: PerspectiveCamera = _
  private var controller: MovementController = _
  private var modelBatch: ModelBatch = _
  private var lights: Environment = _

  override def create(): Unit = {
    world = new World(4, 5, 4)

    val rand = new Random()
    println("generating")
    // perlin noise based generation
    MathUtils.random.setSeed("phoenix".hashCode)
    val heightMap = PerlinNoiseGenerator.generateHeightMap(world.blockSize.xi, world.blockSize.zi, 0, 63, 11)
    var idx = 0
    for (z <- 0 until world.blockSize.zi) {
      for (x <- 0 until world.blockSize.xi) {
        for (y <- 0 until heightMap(idx)) {
          world.set(V3I(x, y, z), if (y < heightMap(idx) - 10) Stone else Dirt)
        }
        idx += 1
      }
    }
    println("generated")
    /*
    world.entities += new Cylinder(
      new V3F(0, heightMap(0) - 0.5f, 0),
      0.5f,
      2f
    )
    */
    val cylinder = new Cylinder(
      new V3F(-10, heightMap(0) + -1, 0),
      0.5f,
      2f
    )
    cylinder.vel = V3F(1, 0, 1)
    world.entities += cylinder
    worldRenderer = new WorldRenderer(world)

    cam = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.01f
    cam.far = 1000
    cam.position.set(V3F(-10, heightMap(0) + 10, -10) toGdx)
    cam.lookAt(0, heightMap(0), 0)
    controller = new MovementController(cam)
    Gdx.input.setInputProcessor(controller)

    modelBatch = new ModelBatch()

    lights = new Environment()
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1f))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))
  }

  override def render(): Unit = {
    world.update()

    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)

    modelBatch.begin(cam)
    modelBatch.render(worldRenderer, lights)
    modelBatch.end()

    controller.update()
  }

  override def resize(width: Int, height: Int): Unit = {
    cam.viewportWidth = width
    cam.viewportHeight = height
    cam.update()
  }

  override def dispose(): Unit = {

  }

}
