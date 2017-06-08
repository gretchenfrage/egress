package com.phoenixkahlo.hellcraft



import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.utils.FirstPersonCameraController
import com.badlogic.gdx.graphics.g3d.{Environment, ModelBatch}
import com.badlogic.gdx.graphics.{GL20, PerspectiveCamera}
import com.badlogic.gdx.{ApplicationAdapter, Gdx}
import com.phoenixkahlo.hellcraft.util.{Directions, Ones, Origin, V3I}

import scala.util.Random

class HellCraft extends ApplicationAdapter {

  private var world: World = _
  private var cam: PerspectiveCamera = _
  private var controller: FirstPersonCameraController = _
  private var modelBatch: ModelBatch = _
  private var lights: Environment = _

  override def create(): Unit = {
    world = new World(10, 10, 10)
    val rand = new Random()

    println("generating")

    for (v <- Origin until world.size)
      if (rand.nextBoolean())
        world.set(v, Stone)

    /*
    for (v <- Origin until world.size)
      world.set(v, Stone)
    */
    //world.set(V3I(0, 0, 0), Stone)
    println("generated")

    cam = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.5f
    cam.far = 1000
    cam.position.set((world.size + V3I(10, 10, 10)).toGdx)
    cam.lookAt(0, 0, 0)
    controller = new FirstPersonCameraController(cam)
    Gdx.input.setInputProcessor(controller)

    modelBatch = new ModelBatch()

    lights = new Environment()
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1f))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))
  }

  override def render(): Unit = {
    Gdx.gl.glClearColor(0.4f, 0.4f, 0.4f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)

    modelBatch.begin(cam)
    modelBatch.render(world, lights)
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
