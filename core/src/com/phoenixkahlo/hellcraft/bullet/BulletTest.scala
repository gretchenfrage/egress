package com.phoenixkahlo.hellcraft.bullet

import java.util.{Random, Scanner}

import com.badlogic.gdx.graphics.g2d.TextureRegion
import com.badlogic.gdx.{Gdx, InputProcessor}
import com.badlogic.gdx.graphics.g3d.utils.FirstPersonCameraController
import com.badlogic.gdx.math.{Matrix4, Vector3}
import com.badlogic.gdx.physics.bullet.Bullet
import com.badlogic.gdx.physics.bullet.collision._
import com.badlogic.gdx.physics.bullet.dynamics.btRigidBody.btRigidBodyConstructionInfo
import com.badlogic.gdx.physics.bullet.dynamics.{btDiscreteDynamicsWorld, btRigidBody, btSequentialImpulseConstraintSolver}
import com.badlogic.gdx.physics.bullet.linearmath.{btDefaultMotionState, btQuaternion, btTransform}
import com.phoenixkahlo.hellcraft.core.Blocks.Brick
import com.phoenixkahlo.hellcraft.core.eval.{ExecCheap, GEval, WEval}
import com.phoenixkahlo.hellcraft.core.{Air, BlockSoup, Chunk, Materials, Terrain, TerrainGrid, TerrainSoup, TerrainUnit, TerrainUnits}
import com.phoenixkahlo.hellcraft.core.graphics.{FreeCube, FreeCubeParams, LineCombiner}
import com.phoenixkahlo.hellcraft.fgraphics.{BasicTriVert, BrickTID, DefaultRenderer, DefaultResourcePack, GenericShader, GlobalRenderData, LineShader, Offset, Render, Renderable, Renderer, ResourcePack, Shader, TerrainShader}
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState}
import com.phoenixkahlo.hellcraft.math.MatrixFactory.Translate
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.collections.spatial.Octree.Octree
import com.phoenixkahlo.hellcraft.util.collections.spatial.{Octree, SpatialTemporalQueue}
import com.phoenixkahlo.hellcraft.util.fields.IDField
import com.phoenixkahlo.hellcraft.util.threading.UniExecutor

import scala.collection.mutable.ArrayBuffer

class BulletTestWorld {
  val noise = Simplex(1f / 8f, 15f)
  implicit val mapping = TerrainUnits
  implicit val exec = ExecCheap

  //val rad = V3I(3, 2, 3)
  val rad = Origin
  val cdomain = rad.neg toAsSet rad
  val tdomain = cdomain.bloat

  val terrains: Map[V3I, Terrain] = {
    val domain = tdomain.toSeq
    domain.map(p => p -> Terrain(p, IDField[TerrainUnit](V3I(16, 16, 16), (i: V3I) => {
      val height = noise((p * 16 + i).flatten)
      val depth = (p.yi * 16 + i.yi) - height
      if (depth >= 0) Air
      else Materials.Grass
    }))).toMap
  }

  println("computed terrain")

  val chunks: Map[V3I, Chunk] = {
    val domain = cdomain.toSeq
    val grid = TerrainGrid(terrains)
    domain.map(p => p -> {
      val ter = terrains(p)
      val bs = BlockSoup(ter, grid).get
      val ts = TerrainSoup(ter, grid).get
      new Chunk(p, ter, ts, bs)
    }).toMap
  }

  println("computed chunks")

  val renders: Seq[Render[_ <: Shader]] =
    chunks.values.toSeq.flatMap(chunk => {
      val r1 = Renderable[TerrainShader](GEval.resourcePack.map(pack => {
        val verts = new ArrayBuffer[BasicTriVert]
        for (v: V3I <- chunk.terrainSoup.indexToVert) {
          val vert: TerrainSoup.Vert = chunk.terrainSoup.verts(v).get
          val tex: TextureRegion = pack(vert.mat.tid)
          verts += BasicTriVert(vert.pos, V4I.ones, V2F(tex.getU, tex.getV), vert.nor)
        }
        (verts, chunk.terrainSoup.indices)
      }))
      val r2 = Renderable[GenericShader](GEval.resourcePack.map(pack => {
        val verts = new ArrayBuffer[BasicTriVert]
        for (vert <- chunk.blockSoup.verts) {
          val tex = pack(vert.block.tid)
          verts += BasicTriVert(vert.pos, V4I.ones, V2F(
            tex.getU + (vert.uvDelta.x / 16f), tex.getV + (vert.uvDelta.y / 16f)
          ), vert.nor)
        }
        (verts, chunk.blockSoup.indices)
      }))

      Seq(
        Render[TerrainShader](r1, Offset.default, true),
        Render[GenericShader](r2, Offset.default, true)
      )
    })

  println("computed chunk renders")

  val tetra: Octree[Render[_ <: Shader]] =
    chunks.values.toSeq
      .flatMap(chunk => chunk.terrainSoup.tetra.get)
      .map(tetra => tetra.average -> Render[LineShader](Renderable[LineShader](GEval(tetra.edges)), Offset.default, true))
      .foldLeft(Octree.bigEmpty: Octree[Render[LineShader]])(_ + _)

  println("created tetra render octree")

}

class BulletTest extends GameState {
  val rand = new Random()

  private var pack: ResourcePack = _
  private var renderer: Renderer = _
  private var controller: FirstPersonCameraController = _

  var chunks: BulletTestWorld = _

  var orbRenderable: Renderable[GenericShader] = _

  private var broadphase: btBroadphaseInterface = _
  private var config: btDefaultCollisionConfiguration = _
  private var dispatcher: btCollisionDispatcher = _
  private var solver: btSequentialImpulseConstraintSolver = _
  private var world: btDiscreteDynamicsWorld = _

  var groundShape: btCollisionShape = _
  var groundCollObject: btCollisionObject = _

  var orbShape: btCollisionShape = _
  var orbMotionState: btDefaultMotionState = _
  var orbConstrInfo: btRigidBodyConstructionInfo = _
  var orbRigidBody: btRigidBody = _

  override def onEnter(driver: GameDriver): Unit = {
    UniExecutor.activate(0, new Thread(_), _.printStackTrace(), SpatialTemporalQueue.timeDoesntMatter, Ones)

    pack = new DefaultResourcePack

    renderer = new DefaultRenderer(pack)

    controller = new FirstPersonCameraController(renderer.cam)
    Gdx.input.setInputProcessor(controller)

    chunks = new BulletTestWorld

    orbRenderable = FreeCube(FreeCubeParams(BrickTID, V4I.ones))

    Bullet.init()

    broadphase = new btDbvtBroadphase
    config = new btDefaultCollisionConfiguration
    dispatcher = new btCollisionDispatcher(config)
    btGImpactCollisionAlgorithm.registerAlgorithm(dispatcher)
    solver =new btSequentialImpulseConstraintSolver
    world = new btDiscreteDynamicsWorld(dispatcher, broadphase, solver, config)
    world.setGravity((Down * 9.8f).toGdx)

    val terrainShapeBuilder = new btCompoundShape(true, 1000000)
    val noTransform = MatrixFactory()
    for (tetra <- chunks.chunks.values.flatMap(_.terrainSoup.tetra.get)) {
      val tetraShape = new btConvexHullShape()
      for (vert <- tetra) {
        tetraShape.addPoint(vert.toGdx)
      }

      terrainShapeBuilder.addChildShape(noTransform, tetraShape)
    }
    groundShape = terrainShapeBuilder
    groundCollObject = new btCollisionObject
    groundCollObject.setCollisionShape(groundShape)
    groundCollObject.setFriction(3)
    world.addCollisionObject(groundCollObject)

    println("computed ground shape")

    val rad = 0.5f
    orbShape = new btSphereShape(rad)
    orbMotionState = new btDefaultMotionState(MatrixFactory(Translate(V3I(8, 50, 8))))
    val mass = 1f
    val inertia = 0//(2f * mass * rad * rad) / 5f
    orbConstrInfo = new btRigidBodyConstructionInfo(mass, orbMotionState, orbShape, Repeated(inertia).toGdx)
    orbRigidBody = new btRigidBody(orbConstrInfo)
    world.addRigidBody(orbRigidBody)

  }

  override def render(): Unit = {
    //println("render")
    world.stepSimulation(1f / 60f, 10)

    val mat = new Matrix4
    orbRigidBody.getMotionState.getWorldTransform(mat)
    val tra = new Vector3
    mat.getTranslation(tra)
    val pos = V3F(tra)

    controller.update()

    val globals = GlobalRenderData(
      V3F(renderer.cam.position), V3F(renderer.cam.direction),
      Up * 80, 1,
      V4I.ones, 90
    )

    val glow = chunks.tetra.within(globals.camPos, 8).map(_._2)
    val block = Render[GenericShader](orbRenderable, Offset(pos))
    renderer(chunks.renders ++ glow :+ block, globals)
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
