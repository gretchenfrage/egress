package com.phoenixkahlo.hellcraft.service.procedures

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.Material
import com.badlogic.gdx.graphics.g3d.model.{MeshPart, Node, NodePart}
import com.badlogic.gdx.graphics.{GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.math.{Matrix4, Vector3}
import com.badlogic.gdx.physics.bullet.Bullet
import com.badlogic.gdx.physics.bullet.collision._
import com.badlogic.gdx.physics.bullet.dynamics.btRigidBody.btRigidBodyConstructionInfo
import com.badlogic.gdx.physics.bullet.dynamics.{btDiscreteDynamicsWorld, btRigidBody, btSequentialImpulseConstraintSolver}
import com.badlogic.gdx.physics.bullet.linearmath.btDefaultMotionState
import com.badlogic.gdx.utils.Disposable
import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.core.event.UE
import com.phoenixkahlo.hellcraft.core.util.StatePinKey
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.math.MatrixFactory.Translate
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.service.PhysicsService._
import com.phoenixkahlo.hellcraft.service.{PhysicsService, ServiceProcedure}
import com.phoenixkahlo.hellcraft.util.debugging.Profiler
import com.phoenixkahlo.hellcraft.util.threading._

import scala.collection.mutable.ArrayBuffer

class PhysicsServiceProcedure extends ServiceProcedure[PhysicsService] {
  var broadphase: btBroadphaseInterface = _
  var collConfig: btDefaultCollisionConfiguration = _
  var dispatcher: btCollisionDispatcher = _
  var solver: btSequentialImpulseConstraintSolver = _
  var dynWorld: btDiscreteDynamicsWorld = _

  val bulletExec: Runnable => Unit = PhysicsServiceProcedure.exec
  val tetraKey = PhysicsServiceProcedure.tetraKey

  //val sequential = new FutSequences(_.run())

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

  def act(body: Body): UE[Fut[Body]] = {
    val p = body.pos / 16 toInts;
    // get the colliders
    UE.chunks(p.neighbors)
      .map(_.map(chunk => chunk.physPin.getImpatient(tetraKey, (chunk, bulletExec))))
      .map(FutSeqFold(_, bulletExec))
      .map(_.map((colliders: Seq[(btCollisionObject, Seq[AnyRef], Seq[Disposable])]) => {
        // add them to the world
        for (collider <- colliders.map(_._1))
          dynWorld.addCollisionObject(collider)

        // create the collider for the body
        val bodyShape: btCollisionShape = body.shape match {
          case Sphere(rad) => new btSphereShape(rad)
          case Capsule(rad, height) => new btCapsuleShape(rad, height)
          case Cylinder(rad, height) => new btCylinderShape(V3F(rad, height / 2, rad).toGdx)
        }
        val bodyMotionState = new btDefaultMotionState(MatrixFactory(Translate(body.pos)))
        val bodyConstrInfo = new btRigidBodyConstructionInfo(body.mass, bodyMotionState, bodyShape, body.inertia.getOrElse(Origin).toGdx)
        val rigidBody = new btRigidBody(bodyConstrInfo)
        rigidBody.setLinearVelocity(body.vel.toGdx)
        rigidBody.setCcdMotionThreshold(0.3f)
        rigidBody.setCcdSweptSphereRadius(body.shape match {
          case Sphere(rad) => rad
          case Capsule(rad, height) => Math.min(rad, height / 2)
          case Cylinder(rad, height) => Math.min(rad, height / 2)
        })

        // add it to the world
        dynWorld.addRigidBody(rigidBody)

        // step the world
        dynWorld.stepSimulation(Delta.dtf, 50)

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
        for (collider <- colliders.map(_._1))
          dynWorld.removeCollisionObject(collider)
        dynWorld.removeRigidBody(rigidBody)
        rigidBody.getMotionState.dispose()
        rigidBody.dispose()
        bodyShape.dispose()
        bodyConstrInfo.dispose()

        // return
        after
      }, bulletExec))
  }
    /*
    FutSeqFold(
      p.neighbors.map(world.chunkFut).map(_.map(_.map(chunk => chunk.physPin.getImpatient(PhysicsServiceProcedure.tetraKey, (chunk, bulletExec))))), bulletExec
    )
      .flatMap(seq => FutSeqFold(seq.flatten, bulletExec))
      .map((colliders: Seq[(btCollisionObject, Seq[AnyRef], Seq[Disposable])]) => {

      // add them to the world
      for (collider <- colliders.map(_._1))
        dynWorld.addCollisionObject(collider)

      // create the collider for the body
      val bodyShape: btCollisionShape = body.shape match {
        case Sphere(rad) => new btSphereShape(rad)
        case Capsule(rad, height) => new btCapsuleShape(rad, height)
        case Cylinder(rad, height) => new btCylinderShape(V3F(rad, height / 2, rad).toGdx)
      }
      val bodyMotionState = new btDefaultMotionState(MatrixFactory(Translate(body.pos)))
      val bodyConstrInfo = new btRigidBodyConstructionInfo(body.mass, bodyMotionState, bodyShape, body.inertia.getOrElse(Origin).toGdx)
      val rigidBody = new btRigidBody(bodyConstrInfo)
      rigidBody.setLinearVelocity(body.vel.toGdx)
      rigidBody.setCcdMotionThreshold(0.3f)
      rigidBody.setCcdSweptSphereRadius(body.shape match {
        case Sphere(rad) => rad
        case Capsule(rad, height) => Math.min(rad, height / 2)
        case Cylinder(rad, height) => Math.min(rad, height / 2)
      })

      // add it to the world
      dynWorld.addRigidBody(rigidBody)

      // step the world
      dynWorld.stepSimulation(Delta.dtf, 50)

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
      for (collider <- colliders.map(_._1))
        dynWorld.removeCollisionObject(collider)
      dynWorld.removeRigidBody(rigidBody)
      rigidBody.getMotionState.dispose()
      rigidBody.dispose()
      bodyShape.dispose()
      bodyConstrInfo.dispose()

      // return
      after
    }, bulletExec)
  }
  */
    /*
    sequential(() => {
      // get the collision objects of the surrounding chunks
      /*
      val p = body.pos / 16 toInts
      val colliders: Seq[btCollisionObject] =
        p.neighbors.flatMap(world.chunk).map(chunk => chunk.physPin.getImpatient(PhysicsServiceProcedure.tetraKey, (chunk))
        */
      val p = body.pos / 16 toInts
      val colliders: Seq[btCollisionObject] =
        p.neighbors.flatMap(world.chunk).map(
          chunk => PartialSyncEval(exec => chunk.physPin.getImpatient(PhysicsServiceProcedure.tetraKey, (chunk, exec)))._1)
      // begin preparing the further collision objects
      /*
      for (near <- (p - V3I(2, 2, 2)) toAsSeq (p + V3I(2, 2, 2))) {
        world.chunk(p).foreach(chunk => chunk.physPin.prepare(tetraKey, chunk, UniExecutor.exec))
      }
      */

      // add them to the world
      for (collider <- colliders)
        dynWorld.addCollisionObject(collider)

      // create the collider for the body
      val bodyShape: btCollisionShape = body.shape match {
        case Sphere(rad) => new btSphereShape(rad)
        case Capsule(rad, height) => new btCapsuleShape(rad, height)
        case Cylinder(rad, height) => new btCylinderShape(V3F(rad, height / 2, rad).toGdx)
      }
      val bodyMotionState = new btDefaultMotionState(MatrixFactory(Translate(body.pos)))
      val bodyConstrInfo = new btRigidBodyConstructionInfo(body.mass, bodyMotionState, bodyShape, body.inertia.getOrElse(Origin).toGdx)
      val rigidBody = new btRigidBody(bodyConstrInfo)
      rigidBody.setLinearVelocity(body.vel.toGdx)

      // add it to the world
      dynWorld.addRigidBody(rigidBody)

      // step the world
      dynWorld.stepSimulation(Delta.dtf, 30)

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
    */

  override def apply[T](call: PhysicsService.Call[T])(implicit exec: (Runnable) => Unit): UE[Fut[T]] = call match {
    case Act(body) => act(body).asInstanceOf[UE[Fut[T]]]
  }

  override def close(): Unit = {
    dynWorld.dispose()
    solver.dispose()
    collConfig.dispose()
    dispatcher.dispose()
    broadphase.dispose()
  }
}

object PhysicsServiceProcedure {
  val bulletSequential = new FutSequence(_.run())
  def exec(task: Runnable): Unit = {
    bulletSequential.apply[Unit](task.run)
  }

  val noTrans = MatrixFactory()
  val tetraKey = new StatePinKey[Fut[(btCollisionObject, Seq[AnyRef], Seq[Disposable])], (Chunk, Runnable => Unit)](
    { case (chunk, any) =>
      Fut({
        val tverts = chunk.terrainSoup.indexToVert.map(v => chunk.terrainSoup.verts(v).get)
        val floats = new ArrayBuffer[Float]
        for (vert <- tverts)
          floats.append(vert.pos.x, vert.pos.y, vert.pos.z)
        for (vert <- chunk.blockSoup.verts)
          floats.append(vert.pos.x, vert.pos.y, vert.pos.z)
        val indices = chunk.terrainSoup.indices ++ chunk.blockSoup.indices.map(s => (s + tverts.length).toShort)
        //println("indices.size = " + indices.size)
        (floats.toArray, indices.toArray)
      }, exec)
        .map({
          case (floats, indices) =>
            val mesh = new Mesh(true, floats.length, indices.length, new VertexAttribute(Usage.Position, 3, "a_position"))
            mesh.setVertices(floats)
            mesh.setIndices(indices)
            mesh
        }, Gdx.app.postRunnable)
        .map(mesh => {
          val (shape, keepAlive, dispose) =
            if (mesh.getNumIndices == 0) {
              val shape = new btEmptyShape()
              (shape, Seq(shape), Seq(shape))
            }
            else {
              // TODO: it may be possible to avoid the mesh system altogether
              val meshPart = new MeshPart("collider", mesh, 0, mesh.getNumIndices, GL20.GL_TRIANGLES)
              val arr = new com.badlogic.gdx.utils.Array[MeshPart]
              arr.add(meshPart)
              val shape = new btBvhTriangleMeshShape(arr)
              (shape, Seq(shape, meshPart), Seq(shape))
            }
          val colObj = new btCollisionObject
          colObj.setCollisionShape(shape)
          (colObj, keepAlive, dispose :+ colObj :+ mesh)
        }, exec)
  }, _.map({
    case (obj, alive, dispose) =>
      dispose.foreach(_.dispose())
  }, Gdx.app.postRunnable))
}