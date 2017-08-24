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

  def getQuads: Option[Seq[Quad]] = None

  def terrainType: TerrainType

}

sealed trait TerrainType

@CarboniteWith(classOf[FieldNode])
case class Densities(pos: V3I, densities: FractionField) extends Terrain {

  def canUpgrade(world: World): Boolean =
    pos.neighbors.forall(world.chunkAt(_).isDefined)

  def upgrade(world: World): Option[Vertices] = {
    if (canUpgrade(world)) {
      val p = Profiler("densities -> vertices upgrade")

      val verts = OptionField(world.resVec, v => {
        val edges: Seq[(V3I, V3I)] = Seq(
          v -> (v + East),
          v -> (v + Up),
          v -> (v + North),

          (v + North) -> (v + East),
          (v + East) -> (v + V3I(1, 1, 0)),
          (v + V3I(1, 0, 1)) -> (v + Ones),

          (v + Up) -> (v + V3I(0, 1, 1)),
          (v + Up) -> (v + V3I(1, 1, 0)),

          (v + North) -> (v + V3I(1, 0, 1)),
          (v + East) -> (v + V3I(1, 0, 1)),

          (v + V3I(0, 1, 1)) -> (v + Ones),
          (v + V3I(1, 1, 0)) -> (v + Ones)
        ).map({ case (v1, v2) => (pos * world.res + v1) -> (pos * world.res + v2) })

        val spoints = new ArrayBuffer[V3F]

        for ((v1, v2) <- edges) {
          if ((world.density(v1).get > 0.5f) ^ (world.density(v2).get > 0.5f)) {
            spoints += ((v1 + v2) / 2)
          }
        }

        if (spoints isEmpty) None
        else {
          val avg = spoints.fold(Origin)(_ + _) / spoints.size
          Some(avg / world.res * 16)
        }
      })
      /*
      val verts = OptionField(world.resVec, i => {
        val p = Profiler("vertex calc " + pos + ", " + i)

        // real world coordinates of the middle of the cube
        val v = (i / world.res + pos) * 16

        val spoints = new ArrayBuffer[V3F]

        p.log()

        val h = 16f / world.res / 2f
        val edges: Seq[(V3F, V3F)] = Seq(
          (v + V3F(-h, -h, -h)) -> (v + V3F(h, -h, -h)),
          (v + V3F(-h, -h, -h)) -> (v + V3F(-h, h, -h)),
          (v + V3F(-h, -h, -h)) -> (v + V3F(-h, -h, h)),

          (v + V3F(-h, -h, h)) -> (v + V3F(-h, h, h)),
          (v + V3F(h, -h, -h)) -> (v + V3F(h, h, -h)),
          (v + V3F(h, -h, h)) -> (v + V3F(h, h, h)),

          (v + V3F(-h, h, -h)) -> (v + V3F(-h, h, h)),
          (v + V3F(-h, h, -h)) -> (v + V3F(h, h, -h)),

          (v + V3F(-h, -h, h)) -> (v + V3F(h, -h, h)),
          (v + V3F(h, -h, -h)) -> (v + V3F(h, -h, h)),

          (v + V3F(-h, h, h)) -> (v + V3F(h, h, h)),
          (v + V3F(h, h, -h)) -> (v + V3F(h, h, h))
        )

        p.log()

        for ((v1, v2) <- edges) {
          if ((world.density(v1).get > 0.5f) != (world.density(v2).get > 0.5f)) {
            spoints += ((v1 + v2) / 2)
          }
        }

        p.log()

        try {
          if (spoints isEmpty) None
          else Some(spoints.fold(Origin)(_ + _) / spoints.size)
        } finally {
          p.log()
          p.printDisc(1)
        }
      })
      */

      p.log()
      p.printDisc(1)

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
    //val dependencies: Seq[V3I] = Seq(pos, pos + Up, pos + East, pos + North)
    val dependencies = pos.neighbors
    dependencies.map(world.chunkAt(_).flatMap(_.terrain.getVertices)).forall(_.isDefined)
  }

  def upgrade(world: World): Option[Quads] = {
    if (canUpgrade(world)) {
      val p = Profiler("vertices -> quads upgrade")

      def vert(v: V3I): Option[V3F] = {
        val global = (pos * world.res) + v
        world.chunkAt(global / world.res floor).get.terrain.getVertices.get.apply(global % world.res)
      }

      val quads = Origin.until(world.resVec)
        .flatMap(v => Seq(
          (v, v + North, v + North + East, v + East),
          (v, v + Up, v + Up + East, v + East),
          (v, v + Up, v + Up + North, v + North)
        ))
        .map({ case (v1, v2, v3, v4) => (vert(v1), vert(v2), vert(v3), vert(v4)) })
        .flatMap({
          case (Some(a), Some(b), Some(c), Some(d)) => Some(Quad(a, b, c, d))
          case _ => None
        })

      p.log()
      p.printDisc(1)

      Some(Quads(pos, densities, vertices, quads))

    } else None
  }

  override def terrainType: TerrainType = Vertices

}

object Vertices extends TerrainType

@CarboniteWith(classOf[FieldNode])
case class Quads(pos: V3I, densities: FractionField, vertices: OptionField[V3F], quads: Seq[Quad]) extends Terrain {
  override def getVertices = Some(vertices)

  override def getQuads = Some(quads)

  override def terrainType: TerrainType = Quads
}

object Quads extends TerrainType