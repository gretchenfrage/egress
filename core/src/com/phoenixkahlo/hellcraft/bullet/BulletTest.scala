package com.phoenixkahlo.hellcraft.bullet

import java.util.{Random, Scanner}

import com.badlogic.gdx.{Gdx, InputProcessor}
import com.badlogic.gdx.graphics.g3d.utils.FirstPersonCameraController
import com.badlogic.gdx.math.{Matrix4, Vector3}
import com.badlogic.gdx.physics.bullet.Bullet
import com.badlogic.gdx.physics.bullet.collision._
import com.badlogic.gdx.physics.bullet.dynamics.btRigidBody.btRigidBodyConstructionInfo
import com.badlogic.gdx.physics.bullet.dynamics.{btDiscreteDynamicsWorld, btRigidBody, btSequentialImpulseConstraintSolver}
import com.badlogic.gdx.physics.bullet.linearmath.{btDefaultMotionState, btQuaternion, btTransform}
import com.phoenixkahlo.hellcraft.core.Blocks.Brick
import com.phoenixkahlo.hellcraft.core.graphics.{FreeCube, FreeCubeParams}
import com.phoenixkahlo.hellcraft.fgraphics.{BrickTID, DefaultRenderer, DefaultResourcePack, GenericShader, GlobalRenderData, Offset, Render, Renderable, Renderer, ResourcePack, Shader}
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState}
import com.phoenixkahlo.hellcraft.math.MatrixFactory.Translate
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.collections.spatial.SpatialTemporalQueue
import com.phoenixkahlo.hellcraft.util.threading.UniExecutor

class BulletTest extends GameState {
  val rand = new Random()

  private var pack: ResourcePack = _
  private var renderer: Renderer = _
  private var controller: FirstPersonCameraController = _

  var orbRenderable: Renderable[GenericShader] = _

  private var broadphase: btBroadphaseInterface = _
  private var config: btDefaultCollisionConfiguration = _
  private var dispatcher: btCollisionDispatcher = _
  private var solver: btSequentialImpulseConstraintSolver = _
  private var world: btDiscreteDynamicsWorld = _

  var groundShape: btCollisionShape = _
  var orbShape: btCollisionShape = _
  var groundMotionState: btDefaultMotionState = _
  var groundConstrInfo: btRigidBodyConstructionInfo = _
  var groundRigidBody: btRigidBody = _

  var orbMotionState: btDefaultMotionState = _
  var orbConstrInfo: btRigidBodyConstructionInfo = _
  var orbRigidBody: btRigidBody = _

  override def onEnter(driver: GameDriver): Unit = {
    UniExecutor.activate(0, new Thread(_), _.printStackTrace(), SpatialTemporalQueue.timeDoesntMatter, Ones)

    pack = new DefaultResourcePack

    renderer = new DefaultRenderer(pack)

    controller = new FirstPersonCameraController(renderer.cam)
    Gdx.input.setInputProcessor(controller)

    orbRenderable = FreeCube(FreeCubeParams(BrickTID, V4I.ones))

    Bullet.init()

    broadphase = new btDbvtBroadphase
    config = new btDefaultCollisionConfiguration
    dispatcher = new btCollisionDispatcher(config)
    btGImpactCollisionAlgorithm.registerAlgorithm(dispatcher)
    solver =new btSequentialImpulseConstraintSolver
    world = new btDiscreteDynamicsWorld(dispatcher, broadphase, solver, config)
    world.setGravity((Down * 9.8f).toGdx)

    val rad = 1f

    groundShape = new btStaticPlaneShape(V3I(0, 1, 0).toGdx, 1)
    orbShape = new btSphereShape(1)
    groundMotionState = new btDefaultMotionState(MatrixFactory(Translate(Up)))
    groundConstrInfo = new btRigidBodyConstructionInfo(0, groundMotionState, groundShape, Origin.toGdx)
    groundRigidBody = new btRigidBody(groundConstrInfo)
    world.addRigidBody(groundRigidBody)

    orbMotionState = new btDefaultMotionState(MatrixFactory(Translate(Up * 25)))
    val mass = 1f
    val inertia = (2f * mass * rad * rad) / 5f
    orbConstrInfo = new btRigidBodyConstructionInfo(mass, orbMotionState, orbShape, Repeated(inertia).toGdx)
    orbRigidBody = new btRigidBody(orbConstrInfo)
    world.addRigidBody(orbRigidBody)

    new Thread(() => {
      val in = new Scanner(System.in)
      while (true) {
        val line = in.nextLine()
        Gdx.app.postRunnable(() => {
          val scan = new Scanner(line)
          scan.next() match {
            case "jump" =>
              val impulse = (Up * scan.nextFloat()).toGdx
              orbRigidBody.activate()
              orbRigidBody.applyCentralImpulse(impulse)
            case str => println("derp: " + str)
          }
        })
      }
    }).start()
  }

  override def render(): Unit = {
    world.stepSimulation(1f / 60f, 10)

    val mat = new Matrix4
    orbRigidBody.getMotionState.getWorldTransform(mat)
    val tra = new Vector3
    mat.getTranslation(tra)
    val pos = V3F(tra)

    controller.update()

    val globals = GlobalRenderData(
      V3F(renderer.cam.position), V3F(renderer.cam.direction),
      Origin, 1,
      V4I.ones, 90
    )

    val renders: Seq[Render[_ <: Shader]] = Seq(
      Render[GenericShader](orbRenderable, Offset(pos))
    )
    renderer(renders, globals)
  }

  override def onResize(width: Int, height: Int): Unit = {
    renderer.onResize(width, height)
  }

  override def onExit(): Unit = {
    renderer.close()
    world.dispose()
    solver.dispose()
    config.dispose()
    dispatcher.dispose()
    broadphase.dispose()
  }
}
