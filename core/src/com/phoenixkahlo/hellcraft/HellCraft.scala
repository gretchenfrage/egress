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

  private var dynamicsWorld: btDynamicsWorld = _
  private var constraintSolver: btConstraintSolver = _
  private var collisionConfig: btDefaultCollisionConfiguration = _
  private var dispatcher: btCollisionDispatcher = _
  private var broadphase: btDbvtBroadphase = _

  private var world: World = _
  private var worldRenderer: WorldRenderer = _
  private var cam: PerspectiveCamera = _
  private var controller: MovementController = _
  private var modelBatch: ModelBatch = _
  private var lights: Environment = _

  private var ball: Ball = _


  override def create(): Unit = {
    Bullet.init()
    collisionConfig = new btDefaultCollisionConfiguration()
    dispatcher = new btCollisionDispatcher(collisionConfig)
    broadphase = new btDbvtBroadphase()
    constraintSolver = new btSequentialImpulseConstraintSolver()
    dynamicsWorld = new btDiscreteDynamicsWorld(dispatcher, broadphase, constraintSolver, collisionConfig)
    //dynamicsWorld.setGravity(new Vector3(0, -9.8f, 0))
    dynamicsWorld.setGravity(new Vector3(0, 0, 0))

    world = new World(4, 5, 4)
    val rand = new Random()

    println("generating")

    /*
    for (cv <- Origin until world.size) {
      val chunk = world(cv).get
      for (v <- Origin until chunk.size) {
        chunk.set(v, BlockDirectory(rand.nextInt(BlockDirectory.blocks.length).toByte))
      }
    }
    */
    /*
    for (cv <- Origin until world.size) {
      val chunk = world.chunk(cv).get
      for (v <- Origin until world.chunkSizeVec) {
        chunk.set(v, if (rand.nextBoolean()) Stone else Dirt)
      }
    }
    */

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

    // balls to the wall
    ball = new Ball(4)
    world.balls.add(ball)
    dynamicsWorld.addRigidBody(ball.body)
    ball.body.translate(new Vector3(0, 70, 0))
    //ball.body.setLinearVelocity(new Vector3(0, 0, 0))
    //b.body.applyCentralForce(new Vector3(10, -10, 10))
    //b.body.applyCentralImpulse(new Vector3(10, 10, 10))

    println("generated")

    worldRenderer = new WorldRenderer(world)

    cam = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.01f
    cam.far = 1000
    cam.position.set((world.size + V3I(-10, 63, -10)).toGdx)
    cam.lookAt(0, 70, 0)
    controller = new MovementController(cam)
    Gdx.input.setInputProcessor(controller)

    modelBatch = new ModelBatch()


    lights = new Environment()
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1f))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))
  }

  override def render(): Unit = {
    val delta = Math.min(1f / 30f, Gdx.graphics.getDeltaTime)
    dynamicsWorld.stepSimulation(delta, 5, 1f / 60f)



    if (ThreadLocalRandom.current.nextFloat() < 1f / 60f) {
      println("impulsing")
      ball.body.applyCentralForce(new Vector3(1000, -1000, 1000))
      ball.body.activate(true)
      /*
      ball.body.applyCentralForce(new Vector3(
        ThreadLocalRandom.current().nextFloat() * 1000,
        ThreadLocalRandom.current().nextFloat() * 1000,
        ThreadLocalRandom.current().nextFloat() * 1000
      ))
      */
    }
    //ball.body.setLinearVelocity(new Vector3(1, -1, 1))
    /*
    val t = new Vector3()
    ball.body.getWorldTransform.getTranslation(t)
    println(t)
    */
    println(ball.body.getLinearVelocity)


    Gdx.gl.glClearColor(0.4f, 0.4f, 0.4f, 1f)
    //Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    import com.badlogic.gdx.Gdx
    import com.badlogic.gdx.graphics.GL20
    Gdx.gl.glClear(
      GL20.GL_COLOR_BUFFER_BIT
        | GL20.GL_DEPTH_BUFFER_BIT
        | (if (Gdx.graphics.getBufferFormat.coverageSampling) GL20.GL_COVERAGE_BUFFER_BIT_NV else 0))
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
