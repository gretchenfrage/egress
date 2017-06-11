package com.phoenixkahlo.hellcraft.prototype

import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.{Environment, ModelBatch}
import com.badlogic.gdx.graphics.{GL20, PerspectiveCamera}
import com.badlogic.gdx.math.MathUtils
import com.badlogic.gdx.{ApplicationAdapter, Gdx, InputMultiplexer}
import com.phoenixkahlo.hellcraft.util._
import other.PerlinNoiseGenerator

import scala.util.Random

class HellCraft extends ApplicationAdapter {

  private var world: World = _
  private var worldRenderer: WorldRenderer = _
  private var cam: PerspectiveCamera = _
  private var camController: MovementController = _
  private var modelBatch: ModelBatch = _
  private var lights: Environment = _

  private var time = 0

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
    /*
    val cylinder = new Cylinder(
      new V3F(-5, heightMap(0) - 1, -4),
      0.5f,
      2f
    )
    cylinder.vel = V3F(0.5f, 0, 0.5f)
    world.entities += cylinder
*/
  /*
    for (_ <- 0 until 100) {
      val cylinder = new Cylinder(
        V3F(-rand.nextInt(20), heightMap(0) + rand.nextInt(20), -rand.nextInt(20)),
        0.5f, 2f
      )
      cylinder.vel = V3F(rand.nextFloat, -rand.nextFloat, rand.nextFloat)
      world.entities += cylinder
    }
    */
    for (_ <- 0 until 100) {
      val cylinder = new Cylinder(
        V3F(rand.nextInt(20), heightMap(0) + 20, rand.nextInt(20)),
        0.5f, 2f
      )
      cylinder.vel = V3F(rand.nextFloat * 5, 0, rand.nextFloat * 5)
      world.entities += cylinder
    }


    worldRenderer = new WorldRenderer(world)

    cam = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.01f
    cam.far = 1000
    cam.position.set(V3F(-10, heightMap(0) + 10, -10) toGdx)
    cam.lookAt(0, heightMap(0), 0)

    val multiplexer = new InputMultiplexer
    camController = new MovementController(cam)
    multiplexer.addProcessor(camController)
    Gdx.input.setInputProcessor(multiplexer)

    modelBatch = new ModelBatch()

    lights = new Environment()
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1f))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))
  }

  override def render(): Unit = {
    if (time > 60 * 3)
      world.update()
    time += 1

    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)

    modelBatch.begin(cam)
    modelBatch.render(worldRenderer, lights)
    modelBatch.end()

    camController.update()
  }



  override def resize(width: Int, height: Int): Unit = {
    cam.viewportWidth = width
    cam.viewportHeight = height
    cam.update()
  }

  override def dispose(): Unit = {

  }

}
