package com.phoenixkahlo.hellcraft.service.procedures

import com.badlogic.gdx.math.{Matrix4, Vector3}
import com.badlogic.gdx.physics.bullet.Bullet
import com.badlogic.gdx.physics.bullet.collision._
import com.badlogic.gdx.physics.bullet.dynamics.btRigidBody.btRigidBodyConstructionInfo
import com.badlogic.gdx.physics.bullet.dynamics.{btDiscreteDynamicsWorld, btRigidBody, btSequentialImpulseConstraintSolver}
import com.badlogic.gdx.physics.bullet.linearmath.btDefaultMotionState
import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.core.util.StatePinKey
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.math.MatrixFactory.Translate
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.service.PhysicsService.{Act, Body, Capsule, Sphere}
import com.phoenixkahlo.hellcraft.service.{PhysicsService, ServiceProcedure, ServiceWorld}
import com.phoenixkahlo.hellcraft.util.debugging.Profiler
import com.phoenixkahlo.hellcraft.util.threading.{Fut, FutSequences, UniExecutor}

class PhysicsServiceProcedure extends ServiceProcedure[PhysicsService] {
  var broadphase: btBroadphaseInterface = _
  var collConfig: btDefaultCollisionConfiguration = _
  var dispatcher: btCollisionDispatcher = _
  var solver: btSequentialImpulseConstraintSolver = _
  var dynWorld: btDiscreteDynamicsWorld = _

  val noTrans = MatrixFactory()
  val tetraKey = new StatePinKey[btCollisionObject, Chunk](chunk => {
    val compound = new btCompoundShape
    for (tetra <- chunk.terrainSoup.tetra.get) {
      val tetraHull = new btConvexHullShape
      for (vert <- tetra)
        tetraHull.addPoint(vert.toGdx)
      compound.addChildShape(noTrans, tetraHull)
    }
    for (i <- Origin untilAsSeq V3I(16, 16, 16)) {
      if (chunk.terrain.grid(i).id < 0) {
        val box = new btBoxShape(V3F(0.5f, 0.5f, 0.5f).toGdx)
        compound.addChildShape(MatrixFactory(Translate(chunk.pos * 16 + i + Repeated(0.5f))), box)
      }
    }
    val obj = new btCollisionObject
    obj.setCollisionShape(compound)
    obj
  }, obj => {
    val shape = obj.getCollisionShape
    obj.dispose()
    shape.dispose()
  })

  val sequential = new FutSequences(_.run())

  override def begin(): Unit = {
    Bullet.init()

    broadphase = new btDbvtBroadphase
    collConfig = new btDefaultCollisionConfiguration
    dispatcher = new btCollisionDispatcher(collConfig)
    btGImpactCollisionAlgorithm.registerAlgorithm(dispatcher)
    solver =new btSequentialImpulseConstraintSolver
    dynWorld = new btDiscreteDynamicsWorld(dispatcher, broadphase, solver, collConfig)
    dynWorld.setGravity((Down * 9.8f).toGdx)
  }

  def act(world: ServiceWorld, body: Body)(implicit exec: (Runnable) => Unit): Fut[Body] = {
    sequential(() => {
      // get the collision objects of the surrounding chunks
      val p = body.pos / 16 toInts
      val colliders: Seq[btCollisionObject] =
        p.neighbors.flatMap(world.chunk).map(chunk => chunk.physPin.getImpatient(tetraKey, chunk))

      // begin preparing the further collision objects
      for (near <- (p - V3I(2, 2, 2)) toAsSeq (p + V3I(2, 2, 2))) {
        world.chunk(p).foreach(chunk => chunk.physPin.prepare(tetraKey, chunk, UniExecutor.exec))
      }

      // add them to the world
      for (collider <- colliders)
        dynWorld.addCollisionObject(collider)

      // create the collider for the body
      val bodyShape: btCollisionShape = body.shape match {
        case Sphere(rad) => new btSphereShape(rad)
        case Capsule(rad, height) => new btCapsuleShape(rad, height)
      }
      val bodyMotionState = new btDefaultMotionState(MatrixFactory(Translate(body.pos)))
      val bodyConstrInfo = new btRigidBodyConstructionInfo(body.mass, bodyMotionState, bodyShape, body.inertia.getOrElse(Origin).toGdx)
      val rigidBody = new btRigidBody(bodyConstrInfo)
      rigidBody.setLinearVelocity(body.vel.toGdx)

      // add it to the world
      dynWorld.addRigidBody(rigidBody)

      // step the world
      dynWorld.stepSimulation(Delta.dtf, 10)

      // extract the new position and velocity
      val pos = {
        val mat = new Matrix4
        rigidBody.getMotionState.getWorldTransform(mat)
        val pos = new Vector3
        mat.getTranslation(pos)
        V3F(pos)
      }
      val vel = V3F(rigidBody.getLinearVelocity)

      // build the new body
      val after = body.copy(pos = pos, vel = vel)

      // clean up from the world and native resources
      for (collider <- colliders)
        dynWorld.removeCollisionObject(collider)
      dynWorld.removeRigidBody(rigidBody)
      rigidBody.getMotionState.dispose()
      rigidBody.dispose()
      bodyShape.dispose()
      bodyConstrInfo.dispose()

      // return
      after
    })
  }

  override def apply[T](world: ServiceWorld, call: PhysicsService.Call[T])(implicit exec: (Runnable) => Unit): Fut[T] = call match {
    case Act(body) => act(world, body).asInstanceOf[Fut[T]]
  }

  override def close(): Unit = {
    dynWorld.dispose()
    solver.dispose()
    collConfig.dispose()
    dispatcher.dispose()
    broadphase.dispose()
  }
}
