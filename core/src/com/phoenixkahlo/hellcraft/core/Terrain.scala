package com.phoenixkahlo.hellcraft.core

import com.phoenixkahlo.hellcraft.carbonite
import com.phoenixkahlo.hellcraft.carbonite._
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.caches.ParamCache
import com.phoenixkahlo.hellcraft.util.debugging.Profiler
import com.phoenixkahlo.hellcraft.util.fields.{FractionField, OptionField}

import scala.collection.mutable.ArrayBuffer

sealed trait Terrain {

  def pos: V3I

  def densities: FractionField

  def getVertices: Option[OptionField[V3F]] = None

  def asFacets: Option[Facets] = None

  def terrainType: TerrainType

}

sealed trait TerrainType

@CarboniteWith(classOf[FieldNode])
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
          val avg = spoints.fold(Origin)(_ + _) / spoints.size
          Some(avg / world.res * 16)
        }
      })
      Some(Vertices(pos, densities, verts))
    } else None
  }

  override def terrainType: TerrainType = Densities

}

object Densities extends TerrainType

@CarboniteWith(classOf[FieldNode])
case class Vertices(pos: V3I, densities: FractionField, vertices: OptionField[V3F]) extends Terrain {

  override def getVertices = Some(vertices)

  def canUpgrade(world: World): Boolean = {
    val dependencies = pos.neighbors
    dependencies.map(world.chunkAt(_).flatMap(_.terrain.getVertices)).forall(_.isDefined)
  }

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


      /*
      val upgraded = Facets(pos, densities, vertices,
        facets(Origin, Up + North, Up, East), facets(Origin, North, North + Up, East),
        facets(Origin, Up + East, Up, North), facets(Origin, East, East + Up, North),
        facets(Origin, North, North + East, Down), facets(Origin, North + East, East, Down)
      )
      */

      val empty = OptionField.empty[Tri](world.resVec)
      val upgraded = Facets(pos, densities, vertices,
        empty, empty,
        empty, empty,
        facets(Origin, North, North + East, Down), facets(Origin, North + East, East, Down)
      )


      Some(upgraded)

    } else None
  }

  override def terrainType: TerrainType = Vertices

}

object Vertices extends TerrainType

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