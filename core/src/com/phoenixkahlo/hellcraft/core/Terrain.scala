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
        val edges = ProtoTerrain.edges map { case (d1, d2) => (pos * world.res + v + d1, pos * world.res + v + d2) }
        // build the edge isopoint set as per the surface nets algorithm, no interpolation
        val isopoints = new ArrayBuffer[V3F]
        for ((v1, v2) <- edges) {
          if (v1.yi < 0 && v1.yi >= 0) {
            println((v1, v2) + " == " + (world.materialGridPoint(v1), world.materialGridPoint(v2)))
          }
          if ((world.materialGridPoint(v1).get == Air) ^ (world.materialGridPoint(v2).get == Air)) {
            isopoints += ((v1 + v2) / 2)
          }
        }
        if (isopoints isEmpty) None
        else {
          // average isopoints and convert to spatial coords
          val vertPos = (isopoints.fold(Origin)(_ + _) / isopoints.size) / world.res * 16
          // find the first material you can that's not air
          val mat = (v to v + Ones).toStream
            .map(vv => world.materialGridPoint(pos * world.res + vv).get).filterNot(_ == Air).head

          Some(Vert(vertPos, mat))
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