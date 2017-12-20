package com.phoenixkahlo.hellcraft.core.entity

import java.io.{DataOutputStream, FileOutputStream}
import java.util.UUID

import com.badlogic.gdx.ApplicationAdapter
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.{ModelInstance, Renderable}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.core.TerrainSoup
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.caches.{DisposableParamCache, ParamCache}
import com.phoenixkahlo.hellcraft.util.collections.ResourceNode
import com.phoenixkahlo.hellcraft.util.collections.spatial.SpatialTemporalQueue
import com.phoenixkahlo.hellcraft.util.fields.{BooleanField, FloatField, OptionField, ShortFieldBuffer}
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}
import other.AppDirs

import scala.collection.JavaConverters
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
/*
case class PreGenCloud(pos: V3F, i: Int, id: UUID) extends Entity with Moveable {
  @transient private lazy val renderUnit =
    new ParamCache[ResourcePack, Seq[RenderUnit]](pack => Seq(new CloudUnit(pos, i, pack)))

  override def updatePos(newPos: V3F): Entity = copy(pos = newPos)

  override def renderables(pack: ResourcePack): Seq[RenderUnit] = renderUnit(pack)
}

class CloudUnit(pos: V3F, index: Int, pack: ResourcePack) extends RenderUnit {
  val renderables = {
    val instance = new ModelInstance(pack.cloud(index))
    instance.transform.setTranslation(pos toGdx)

    val array = new com.badlogic.gdx.utils.Array[Renderable]()
    val pool = new Pool[Renderable]() {
      override def newObject(): Renderable = new Renderable
    }
    instance.getRenderables(array, pool)
    for (renderable <- JavaConverters.iterableAsScalaIterable(array)) {
      renderable.userData = TerrainSID
    }

    JavaConverters.iterableAsScalaIterable(array).toSeq
  }

  override def apply(interpolation: Interpolation): Seq[Renderable] = renderables

  override def resources: Seq[ResourceNode] = Seq.empty
}
*/
/*
class ProceduralCloud(override val pos: V3F, val seed: Long, val size: V3I, val iso: Float, override val id: UUID, val minRad: Float, val maxRad: Float, val balls: Int, val scale: V3F, @transient lastRenderer: CloudRenderer) extends Entity with Moveable {
  val renderer: CloudRenderer =
    if (lastRenderer != null) lastRenderer
    else new CloudRenderer(this)

  override def updatePos(newPos: V3F): Entity =
    new ProceduralCloud(newPos, seed, size, iso, id, minRad, maxRad, balls, Ones, renderer)

  override def renderables(pack: ResourcePack): Seq[RenderUnit] =
    renderer(pack)
}

class CloudGenerator extends ApplicationAdapter {

  override def create(): Unit = {
    UniExecutor.activate(0, new Thread(_), _.printStackTrace(), SpatialTemporalQueue.secondEqualsMeter )
    val rand = new Random
    val pack = new DefaultResourcePack
    AppDirs.dataDir("egress").resolve("clouds").toFile.mkdirs()
    for (i <- Stream.iterate(0)(_ + 1)) {
      println("generating cloud " + i)

      val cloud = new ProceduralCloud(Origin, rand.nextLong(), V3I(rand.nextInt(30) + 10, rand.nextInt(20) + 5, rand.nextInt(30) + 10), 0.7f, UUID.randomUUID(), 3.5f, 6f, 15, V3F(10, 5, 10), null)

      println("generated verts and indices")
      val (verts, indices) = cloud.renderer.mesh(pack).await

      val vertFile = AppDirs.dataDir("egress").resolve("clouds").resolve(i + "_verts.dat").toFile
      vertFile.createNewFile()
      val vertOut = new DataOutputStream(new FileOutputStream(vertFile))
      for (f <- verts) {
        vertOut.writeFloat(f)
      }
      vertOut.close()
      println("wrote vertices to file")

      val indexFile = AppDirs.dataDir("egress").resolve("clouds").resolve(i + "_indices.dat").toFile
      indexFile.createNewFile()
      val indexOut = new DataOutputStream(new FileOutputStream(indexFile))
      for (i <- indices) {
        indexOut.writeShort(i)
      }
      indexOut.close()
      println("wrote indices to file")

      println("done with cloud " + i)
    }
  }



}

class CloudRenderer(cloud: ProceduralCloud) {
  // garbage-collectable vertex array that's computed in background
  val mesh = new ParamCache[ResourcePack, Fut[(Seq[Float], Seq[Short])]](pack => Fut({
    // define density functions
    val (minRad, maxRad) = (cloud.minRad, cloud.maxRad)
    val ballPoss = RNG.v3fs(RNG(cloud.seed)).map(_ ** cloud.size + cloud.size)
    println(ballPoss.take(cloud.balls).to[Vector])
    val ballRads = RNG.floats(RNG(cloud.seed ^ 298729876345987L)).map(_ * minRad + (maxRad - minRad))
    val balls = ballPoss.zip(ballRads).take(cloud.balls)
    val scrunch = V3F(1, 0.6f, 1)
    def metaball(v: V3F): Float =
      balls.map({ case (pos, rad) => (rad * rad) / ((v - pos) \\ scrunch).magnitudeSqrd }).sum

    val noise = Simplex(0.1f, 1)
    val simpFrac = 1.5f
    def simplex(v: V3F): Float =
      noise(v) / simpFrac + (1f - 1f / simpFrac)

    val a = 1
    val b = cloud.size * 1.5f
    val c = cloud.size
    def gaussian(v: V3F): Float =
      a * Math.pow(Math.E, -Math.pow(v.x - b.x, 2) / (2 * c.x * c.x)).toFloat *
      a * Math.pow(Math.E, -Math.pow(v.y - b.y, 2) / (2 * c.y * c.y)).toFloat *
      a * Math.pow(Math.E, -Math.pow(v.z - b.z, 2) / (2 * c.z * c.z)).toFloat

    def density(v: V3F): Float =
      simplex(v) * metaball(v) * gaussian(v)

    def contained(v: V3F): Boolean =
      density(v) > cloud.iso

    def direction(v: V3F): V3F =
      Directions().map(d => d * density(d * 0.01f + v)).fold(Origin)(_ + _)./(6).normalize.neg

    // generate vertex field
    case class Vert(pos: V3F, nor: V3F)
    val vertField = OptionField[Vert](cloud.size* 3, v => {
      val edges = TerrainSoup.edges map { case (d1, d2) => (v + d1, v + d2) }
      val isopoints = new ArrayBuffer[V3F]
      for ((v1, v2) <- edges) {
        if (contained(v1) ^ contained(v2)) {
          isopoints += ((v1 + v2) / 2)
        }
      }
      if (isopoints isEmpty) None
      else {
        val pos = isopoints.fold(Origin)(_ + _) / isopoints.size
        val dir = direction(pos)
        Some(Vert(pos, dir))
      }
    })

    // assign indices
    val indexToVert = new ArrayBuffer[V3I]
    val vertToIndex = new ShortFieldBuffer(cloud.size* 3)
    for ((v, i) <- (Origin untilAsSeq cloud.size* 3).filter(vertField(_) isDefined).zipWithIndex) {
      indexToVert += v
      vertToIndex(v) = i toShort
    }

    println(indexToVert.size + " vertices generated")

    // find facets
    val indices = new ArrayBuffer[Short]
    for {
      v <- Origin untilAsSeq cloud.size* 3 - Ones
      (d1, d2, d3, dir) <- TerrainSoup.deltas
    } yield (vertField(v), vertField(v + d1), vertField(v + d2), vertField(v + d3)) match {
      case (Some(vert1), Some(vert2), Some(vert3), Some(vert4)) =>
        // verts 1, 2, 3
        if ((((vert2.pos - vert1.pos) cross (vert3.pos - vert1.pos)) dot
          direction((vert1.pos + vert2.pos + vert3.pos) / 3)) > 0)
          indices.append(vertToIndex(v), vertToIndex(v + d1), vertToIndex(v + d2))
        else
          indices.append(vertToIndex(v), vertToIndex(v + d2), vertToIndex(v + d1))
        // verts 1, 3, 4
        if ((((vert3.pos - vert1.pos) cross (vert4.pos - vert1.pos)) dot
          direction((vert1.pos + vert3.pos + vert4.pos) / 3)) > 0)
          indices.append(vertToIndex(v), vertToIndex(v + d2), vertToIndex(v + d3))
        else
          indices.append(vertToIndex(v), vertToIndex(v + d3), vertToIndex(v + d2))
      case _ =>
    }

    // generate vertex data array
    val color = new Color(1, 1, 1, 1).toFloatBits
    val tex = pack.sheetRegion(GrayTID)
    val (u1, v1) = (tex.getU, tex.getV)

    val vertices = new ArrayBuffer[Float]
    for (v <- indexToVert) {
      val vert = vertField(v).get
      val pos = vert.pos ** cloud.scale - (cloud.size ** cloud.scale *  1.5f)
      vertices.append(
        pos.x, pos.y, pos.z,
        color,
        u1, v1,
        vert.nor.x, vert.nor.y, vert.nor.z
      )
    }

    println("cloud generated")
    (vertices, indices)
  }, UniExecutor.exec(cloud.pos)))

  // resource-owning renderable that must be outputted as resource node whenever existing
  val renderable = new DisposableParamCache[ResourcePack, Renderable](pack => {
    val (verts, indices) = mesh(pack).await
    val m = new Mesh(true, verts.size, indices.size,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
      new VertexAttribute(Usage.TextureCoordinates, 2, "a_texCoord0"),
      new VertexAttribute(Usage.Normal, 3, "a_normal")
    )
    m.setVertices(verts.toArray)
    m.setIndices(indices.toArray)
    val r = new Renderable
    r.meshPart.mesh = m
    r.material = new com.badlogic.gdx.graphics.g3d.Material
    r.meshPart.offset = 0
    r.meshPart.size = indices.size
    r.meshPart.primitiveType = GL20.GL_TRIANGLES
    r.userData = TerrainSID
    r
  }, _.meshPart.mesh.dispose())

  // garbage collectable render unit and associated resource. manages the renderable's state. the param cache is to
  // bind it to the resource pack and ensure there's only one of it.
  val units = new ParamCache[ResourcePack, Seq[RenderUnit]](pack => {
    val resource = new ResourceNode {
      override def dependencies: Seq[ResourceNode] = Seq.empty

      override def dispose(): Unit = renderable.invalidate
    }
    val unit = new RenderUnit {
      override def apply(interpolation: Interpolation): Seq[Renderable] =
        if (mesh(pack).query.isDefined) Seq(renderable(pack))
        else Seq.empty

      override def resources: Seq[ResourceNode] =
        if (renderable.isDefined) Seq(resource)
        else Seq.empty
    }
    Seq(unit)
  })

  def apply(pack: ResourcePack): Seq[RenderUnit] = units(pack)
}
*/