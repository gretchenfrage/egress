package com.phoenixkahlo.hellcraft.core

import com.phoenixkahlo.hellcraft.carbonite
import com.phoenixkahlo.hellcraft.carbonite._
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.Vertices.Vert
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.caches.ParamCache
import com.phoenixkahlo.hellcraft.util.debugging.Profiler
import com.phoenixkahlo.hellcraft.util.fields.{FractionField, OptionField, ShortFieldBuffer}

import scala.collection.mutable.ArrayBuffer

/**
  * Terrain represents the world's terrain at a center chunk. It is directly owned by a chunk, and encapsulates the
  * iso-surface logic. Terrain can exist in three states, each of which is an upgrade of the other. A Terrain can
  * be upgraded if its adjacent chunks' terrains are equal or greater in tier.
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
sealed trait Terrain {

  def pos: V3I

  def densities: FractionField

  def getVertices: Option[OptionField[Vertices.Vert]] = None

  def asMeshable: Option[Meshable] = None

  def terrainType: TerrainType

}

sealed trait TerrainType

@CarboniteFields
case class Densities(pos: V3I, densities: FractionField) extends Terrain {

  def canUpgrade(world: World): Boolean =
    pos.neighbors.forall(world.chunkAt(_).isDefined)

  def upgrade(world: World): Option[Vertices] = {
    if (canUpgrade(world)) {
      val verts = OptionField(world.resVec, v => {
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

        val t = 0.5f
        for ((v1, v2) <- edges) {
          val d1 = world.density(v1).get
          val d2 = world.density(v2).get
          if ((d1 > t) ^ (d2 > t)) {
            val delta = (t - d1) / (d2 - d1)
            spoints += (v1 + ((v2 - v1) * delta))
          }
        }

        if (spoints isEmpty) None
        else {
          val p = (spoints.fold(Origin)(_ + _) / spoints.size) / world.res * 16
          val n = world.sampleDirection(p).get
          Some(Vert(p, n))
        }
      })
      Some(Vertices(pos, densities, verts))
    } else None
  }

  override def terrainType: TerrainType = Densities

}

object Densities extends TerrainType

@CarboniteFields
case class Vertices(pos: V3I, densities: FractionField, vertices: OptionField[Vert]) extends Terrain {

  override def getVertices = Some(vertices)

  def canUpgrade(world: World): Boolean =
      pos.neighbors.map(world.chunkAt(_).flatMap(_.terrain.getVertices)).forall(_.isDefined)

  /*
  def upgrade(world: World): Option[Facets] = {
    if (canUpgrade(world)) {
      def vert(v: V3I): Option[V3F] = {
        val global = (pos * world.res) + v
        world.chunkAt(global / world.res floor).get.terrain.getVertices.get.apply(global % world.res)
      }

      def facets(d1: V3I, d2: V3I, d3: V3I, dir: Direction): OptionField[Tri] =
        OptionField(world.resVec, v => {
          (vert(v + d1), vert(v + d2), vert(v + d3)) match {
            case (Some(a), Some(b), Some(c)) =>
              val tri = Tri(a, b, c)
              if (world.sampleDirection(tri.center).get.dot(dir) > 0) Some(tri)
              else Some(tri.reverse)
            case _ => None
          }
        })

      val upgraded = Facets(pos, densities, vertices,
        facets(Origin, Up + North, Up, East), facets(Origin, North, North + Up, East),
        facets(Origin, Up + East, Up, North), facets(Origin, East, East + Up, North),
        facets(Origin, North, North + East, Down), facets(Origin, North + East, East, Down)
      )


      Some(upgraded)

    } else None
  }
  */

  def upgrade(world: World): Option[Meshable] = {
    if (canUpgrade(world)) {
      // first, generate the vertex-index maps, in both directions
      val vertMap = new ArrayBuffer[V3I]
      val vertMapInv = new ShortFieldBuffer(world.resVec)
      var index: Short = 0

      for (v <- Origin until world.resVec) {
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

      val deltas = Seq(
        (North, North + East, East),
        (Up, Up + North, North),
        (Up, Up + East, East)
      )

      for (v <- Origin until world.resVec) {
        for ((d1, d2, d3) <- deltas) {
          if (vertices(v).isDefined &&
            vertices(v + d1).isDefined &&
            vertices(v + d2).isDefined &&
            vertices(v + d3).isDefined) {
            indices.append(vertMapInv(v), vertMapInv(v + d1), vertMapInv(v + d2))
            indices.append(vertMapInv(v), vertMapInv(v + d2), vertMapInv(v + d1))
            indices.append(vertMapInv(v), vertMapInv(v + d2), vertMapInv(v + d3))
            indices.append(vertMapInv(v), vertMapInv(v + d3), vertMapInv(v + d2))
          }
        }
      }

      Some(Meshable(pos, densities, vertices, vertMap, indices))
    } else None
  }

  override def terrainType: TerrainType = Vertices

}

object Vertices extends TerrainType {
  case class Vert(p: V3F, n: V3F)
}

@CarboniteFields
case class Meshable(pos: V3I, densities: FractionField, vertices: OptionField[Vert], vertMap: Seq[V3I],
                    indices: Seq[Short]) extends Terrain {
  override def terrainType: TerrainType = Meshable

  override def getVertices = Some(vertices)

  override def asMeshable = Some(this)
}

object Meshable extends TerrainType

/*
@CarboniteWith(classOf[FieldNode])
case class Facets(pos: V3I, densities: FractionField, vertices: OptionField[V3F],
                  a: OptionField[Tri], b: OptionField[Tri],
                  c: OptionField[Tri], d: OptionField[Tri],
                  e: OptionField[Tri], f: OptionField[Tri]) extends Terrain with Iterable[Tri] {
  override def getVertices = Some(vertices)

  override def asFacets = Some(this)

  override def terrainType: TerrainType = Facets

  override def iterator: Iterator[Tri] =
    Iterator(a, b, c, d, e, f).map(_.iterator).flatten
}

object Facets extends TerrainType
*/