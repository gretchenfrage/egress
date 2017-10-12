package com.phoenixkahlo.hellcraft.core

import com.phoenixkahlo.hellcraft.carbonite
import com.phoenixkahlo.hellcraft.carbonite._
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.CompleteTerrain.Vert
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.caches.ParamCache
import com.phoenixkahlo.hellcraft.util.debugging.Profiler
import com.phoenixkahlo.hellcraft.util.fields._

import scala.collection.mutable.ArrayBuffer

sealed trait Terrain {
  def pos: V3I

  def materials: IDField[Material]

  def isComplete: Boolean

  def asComplete: Option[CompleteTerrain]
}

@CarboniteFields
case class ProtoTerrain(pos: V3I, materials: IDField[Material]) extends Terrain {
  override def asComplete: Option[CompleteTerrain] = None

  override def isComplete: Boolean = false

  def canComplete(world: World): Boolean = pos.neighbors.forall(world.chunkAt(_).isDefined)

  /**
    * If all neighboring chunks are defined, this function will upgrade this ProtoTerrain into a CompleteTerrain as per
    * the surface nets algorithm.
    */
  def complete(world: World): Option[CompleteTerrain] =
    if (canComplete(world)) {
      // generate the vertex field with an overshoot of <1, 1, 1> to connect the chunks
      val verts = OptionField[Vert](world.resVec + Ones, v => {
        // pairs of grid coords to test
        val edges = ProtoTerrain.edges map { case (d1, d2) => (pos * world.res + d1, pos * world.res + d2) }
        // build the edge isopoint set as per the surface nets algorithm, no interpolation
        val isopoints = new ArrayBuffer[V3F]
        for ((v1, v2) <- edges) {
          if ((world.materialGridPoint(v1).get == Air) ^ (world.materialGridPoint(v2).get == Air))
            isopoints += ((v1 + v2) / 2)
        }
        if (isopoints isEmpty) None
        else {
          // average isopoints and convert to spatial coords
          val pos = (isopoints.fold(Origin)(_ + _) / isopoints.size) / world.res * 16
          // find the first material you can that's not air
          val mat = (v to v + Ones).toStream.map(world.materialGridPoint(_).get).filterNot(_ == Air).head

          Some(Vert(pos, mat))
        }
      })

      // generate the vertex-index maps in both direction
      val indexToVert = new ArrayBuffer[V3I]
      val vertToIndex = new ShortFieldBuffer(verts.sizeVec)
      for ((v, i) <- (Origin until verts.sizeVec).filter(verts(_) isDefined).zipWithIndex) {
        indexToVert += v
        vertToIndex(v) = i toShort
      }

      // find facets and generate the indices
      val indices = new ArrayBuffer[Short]
      for {
        v <- Origin until verts.sizeVec - Ones
        (d1, d2, d3) <- ProtoTerrain.deltas
      } yield (verts(v), verts(v + d1), verts(v + d2), verts(v + d3)) match {
        case (Some(vert1), Some(vert2), Some(vert3), Some(vert4)) =>
          // verts 1, 2, 3
          if ((((vert2.pos - vert1.pos) cross (vert3.pos - vert1.pos)) dot
            world.sampleDirection((vert1.pos + vert2.pos + vert3.pos) / 3).get.neg) > 0)
            indices.append(vertToIndex(v), vertToIndex(v + d1), vertToIndex(v + d2))
          else
            indices.append(vertToIndex(v), vertToIndex(v + d2), vertToIndex(v + d1))
          // verts 1, 3, 4
          if ((((vert3.pos - vert1.pos) cross (vert4.pos - vert1.pos)) dot
            world.sampleDirection((vert1.pos + vert3.pos + vert4.pos) / 3).get.neg) > 0)
            indices.append(vertToIndex(v), vertToIndex(v + d2), vertToIndex(v + d3))
          else
            indices.append(vertToIndex(v), vertToIndex(v + d3), vertToIndex(v + d2))

        case _ =>
      }

      Some(CompleteTerrain(pos, materials, verts, indices, indexToVert, vertToIndex.immutabilize))
    } else None
}
object ProtoTerrain {
  val edges: Seq[(V3I, V3I)] = Seq(
    Origin -> West,
    Origin -> (Origin + Up),
    Origin -> (Origin + North),

    North -> West,
    West -> (Origin + V3I(1, 1, 0)),
    V3I(1, 0, 1) -> (Origin + Ones),

    Up -> V3I(0, 1, 1),
    Up -> V3I(1, 1, 0),

    North -> V3I(1, 0, 1),
    West -> V3I(1, 0, 1),

    V3I(0, 1, 1) -> Ones,
    V3I(1, 1, 0) -> Ones
  )
  val deltas = Seq(
    (North, North + West, West),
    (Up, Up + North, North),
    (Up, Up + West, West)
  )
}

@CarboniteFields
case class CompleteTerrain(pos: V3I, materials: IDField[Material],
                           verts: OptionField[Vert], indices: Seq[Short], indexToVert: Seq[V3I], vertToIndex: ShortField) extends Terrain {
  override def isComplete: Boolean = true

  override def asComplete: Option[CompleteTerrain] = Some(this)
}
object CompleteTerrain {
  @CarboniteFields case class Vert(pos: V3F, mat: Material)
}
/**
  * Terrain represents the world's terrain at a certain chunk. It is directly owned by a chunk, and encapsulates the
  * iso-surface logic. Terrain can exist in three states, each of which is an upgrade of the former. A Terrain can
  * be queried for whether it's able to upgrade itself in a given world.
  *
  * The first type is Densities, which is produced by the world generator. It contains a grid of density values,
  * from 0 to 1.
  *
  * The second type is Vertices. It contains optional vertex data for each voxel, as per the surface nets algorithm.
  *
  * The second type is Meshable. It contains a vertex array, and an index array, similar to how meshes work in OpenGL.
  * This includes the triangles being represented in clockwise-front order. This state is finally usable, for both
  * graphics and physics systems.
  *
  * Each class of Terrain has a companion object, which extends the sealed trait TerrainType. A terrain object can
  * be queried for its TerrainType. This allows the neat classification (think grouping and filtering) of Terrain
  * objects without using reflection.
  */
/*
sealed trait Terrain {

  def pos: V3I

  def materials: ByteField

  def densities: FloatField

  def getVertices: Option[OptionField[Vertices.Vert]] = None

  def asMeshable: Option[Meshable] = None

  def terrainType: TerrainType

}

sealed trait TerrainType

@CarboniteFields
case class Densities(pos: V3I, materials: ByteField, densities: FloatField) extends Terrain {

  def canUpgrade(world: World): Boolean =
    pos.neighbors.forall(world.chunkAt(_).isDefined)

  def upgrade(world: World): Option[Vertices] = {
    if (canUpgrade(world)) {
      val verts = OptionField(world.resVec + Ones, v => {
        val edges: Seq[(V3I, V3I)] = Seq(
          v -> (v + West),
          v -> (v + Up),
          v -> (v + North),

          (v + North) -> (v + West),
          (v + West) -> (v + V3I(1, 1, 0)),
          (v + V3I(1, 0, 1)) -> (v + Ones),

          (v + Up) -> (v + V3I(0, 1, 1)),
          (v + Up) -> (v + V3I(1, 1, 0)),

          (v + North) -> (v + V3I(1, 0, 1)),
          (v + West) -> (v + V3I(1, 0, 1)),

          (v + V3I(0, 1, 1)) -> (v + Ones),
          (v + V3I(1, 1, 0)) -> (v + Ones)
        ).map({ case (v1, v2) => (pos * world.res + v1) -> (pos * world.res + v2) })

        val spoints = new ArrayBuffer[V3F]

        val t = 0
        for ((v1, v2) <- edges) {
          val d1 = world.densityGridPoint(v1).get
          val d2 = world.densityGridPoint(v2).get
          if ((d1 > t) ^ (d2 > t)) {
            val delta = Math.min(Math.max((t - d1) / (d2 - d1), 0.2f), 0.8f)
            spoints += (v1 + ((v2 - v1) * delta))
          }
        }

        if (spoints isEmpty) None
        else {
          val p = (spoints.fold(Origin)(_ + _) / spoints.size) / world.res * 16
          val n = world.sampleDirection(p).get.neg
          var mat = world.materialGridPoint(pos * world.res + v).get

          // attempt to make the material not be air
          if (mat == Air)
            mat = (pos * world.res + v).neighbors.map(world.materialGridPoint(_).get).fold(Air)(
              (mat1, mat2) => if (mat1 == Air) mat2 else mat1)

          Some(Vert(p, n, mat))
        }
      })
      Some(Vertices(pos, materials, verts, densities))
    } else None
  }

  override def terrainType: TerrainType = Densities

}

object Densities extends TerrainType

@CarboniteFields
case class Vertices(pos: V3I, materials: ByteField, vertices: OptionField[Vert], densities: FloatField) extends Terrain {

  override def getVertices = Some(vertices)

  def canUpgrade(world: World): Boolean =
    pos.neighbors.forall(world.chunkAt(_).isDefined)

  def upgrade(world: World): Option[Meshable] = {
    if (canUpgrade(world)) {
      // first, generate the vertex-index maps, in both directions
      val vertMap = new ArrayBuffer[V3I]
      val vertMapInv = new ShortFieldBuffer(vertices.sizeVec)
      var index: Short = 0

      for (v <- Origin until vertices.sizeVec) {
        vertices(v) match {
          case Some(vert) =>
            vertMap += v
            vertMapInv(v) = index
            index = (index + 1).toShort
          case _ =>
        }
      }

      // then, find facets and build the index sequence
      val indices = new ArrayBuffer[Short]

      // all deltas must be in the positive direction to make use of vertices overlap and avoid patches
      val deltas = Seq(
        (North, North + West, West),
        (Up, Up + North, North),
        (Up, Up + West, West)
      )

      for (v <- Origin until world.resVec) {
        for ((d1, d2, d3) <- deltas) {
          (vertices(v), vertices(v + d1), vertices(v + d2), vertices(v + d3)) match {
            case (Some(vert1), Some(vert2), Some(vert3), Some(vert4)) =>
              if ((((vert2.p - vert1.p) cross (vert3.p - vert1.p)) dot
                world.sampleDirection((vert1.p + vert2.p + vert3.p) / 3).get.neg) > 0)
                indices.append(vertMapInv(v), vertMapInv(v + d1), vertMapInv(v + d2))
              else
                indices.append(vertMapInv(v), vertMapInv(v + d2), vertMapInv(v + d1))

              if ((((vert3.p - vert1.p) cross (vert4.p - vert1.p)) dot
                world.sampleDirection((vert1.p + vert3.p + vert4.p) / 3).get.neg) > 0)
                indices.append(vertMapInv(v), vertMapInv(v + d2), vertMapInv(v + d3))
              else
                indices.append(vertMapInv(v), vertMapInv(v + d3), vertMapInv(v + d2))


            case _ =>
          }
        }
      }

      Some(Meshable(pos, materials, densities, vertices, vertMap, indices))
    } else None
  }

  override def terrainType: TerrainType = Vertices

}

object Vertices extends TerrainType {
  @CarboniteFields
  case class Vert(p: V3F, n: V3F, material: Material)
}

@CarboniteFields
case class Meshable(pos: V3I, materials: ByteField, densities: FloatField, vertices: OptionField[Vert],
                    vertMap: Seq[V3I], indices: Seq[Short]) extends Terrain {
  override def terrainType: TerrainType = Meshable

  override def getVertices = Some(vertices)

  override def asMeshable = Some(this)
}

object Meshable extends TerrainType
*/

