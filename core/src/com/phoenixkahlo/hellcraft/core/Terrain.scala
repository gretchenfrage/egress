package com.phoenixkahlo.hellcraft.core

import com.phoenixkahlo.hellcraft.carbonite
import com.phoenixkahlo.hellcraft.carbonite._
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.CompleteTerrain.{BVert, TVert}
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.caches.ParamCache
import com.phoenixkahlo.hellcraft.util.debugging.Profiler
import com.phoenixkahlo.hellcraft.util.fields._

import scala.collection.mutable.ArrayBuffer

sealed trait Terrain {
  def pos: V3I

  def grid: IDField[TerrainUnit]

  def isComplete: Boolean

  def asComplete: Option[CompleteTerrain]
}

@CarboniteFields
case class ProtoTerrain(pos: V3I, grid: IDField[TerrainUnit]) extends Terrain {
  override def asComplete: Option[CompleteTerrain] = None

  override def isComplete: Boolean = false

  def canComplete(world: World): Boolean = pos.neighbors.forall(world.chunkAt(_).isDefined)

  /**
    * If all neighboring chunks are defined, this function will upgrade this ProtoTerrain into a CompleteTerrain as per
    * the surface nets algorithm.
    */
  def complete(world: World): Option[CompleteTerrain] =
    if (canComplete(world)) {
      val offset = pos * world.res
      
      // PART 1: generating the terrain mesh with the surface nets algorithm

      // generate the vertex field with an overshoot of <1, 1, 1> to connect the chunks
      val verts = OptionField[TVert](world.resVec + Ones, v => {
        // pairs of grid coords to test
        val edges = ProtoTerrain.edges map { case (d1, d2) => (offset + v + d1, offset + v + d2) }
        // build the edge isopoint set as per the surface nets algorithm, no interpolation
        val isopoints = new ArrayBuffer[V3F]
        for ((v1, v2) <- edges) {
          if ((world.terrainGridPoint(v1).get.id > 0) ^ (world.terrainGridPoint(v2).get.id > 0)) {
            isopoints += ((v1 + v2) / 2)
          }
        }
        if (isopoints isEmpty) None
        else {
          // average isopoints and convert to spatial coords
          val vertPos = (isopoints.fold(Origin)(_ + _) / isopoints.size) / world.res * 16
          // find the first material you can that's not air
          val mat = (v to v + Ones).toStream
            .map(vv => world.terrainGridPoint(offset + vv).get).filterNot(_ == Air).head

          Some(TVert(vertPos, mat))
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

      // PART 2: generating the blocks mesh
      val n = -0.5f
      val p = 0.5f

      val bverts = new ArrayBuffer[BVert]
      val bindices = new ArrayBuffer[Short]
      var bindex = 0
      for {
        v <- Origin until world.resVec
        d <- Directions()
      } yield{
        val visible = (grid(v).id, world.terrainGridPoint(offset + v + d).get.id) match {
          case (t, c) if t >= 0 => false // if target isn't block, isn't visible
          case (t, c) if c < 0 => false // if cover is block, isn't visible
          case _ => true // in all other cases, block is visisble
        }
        if (visible) {
          val block = grid(v).asInstanceOf[Block]
          d match {
            case South =>
              bverts += BVert(offset + v + V3F(n, n, n), block, V2I(1, 0), d)
              bverts += BVert(offset + v + V3F(p, n, n), block, V2I(0, 0), d)
              bverts += BVert(offset + v + V3F(p, p, n), block, V2I(0, 1), d)
              bverts += BVert(offset + v + V3F(n, p, n), block, V2I(1, 1), d)
              bindices.append(Seq(2, 1, 0, 3, 2, 0).map(i => (i + bindex).toShort): _*)
              bindex += 4
            case North =>
              bverts += BVert(offset + v + V3F(n, n, p), block, V2I(0, 0), d)
              bverts += BVert(offset + v + V3F(p, n, p), block, V2I(1, 0), d)
              bverts += BVert(offset + v + V3F(p, p, p), block, V2I(1, 1), d)
              bverts += BVert(offset + v + V3F(n, p, p), block, V2I(0, 1), d)
              bindices.append(Seq(1, 2, 0, 2, 3, 0).map(i => (i + bindex).toShort): _*)
              bindex += 4
            case West =>
              bverts += BVert(offset + v + V3F(p, n, n), block, V2I(1, 0), d)
              bverts += BVert(offset + v + V3F(p, n, p), block, V2I(0, 0), d)
              bverts += BVert(offset + v + V3F(p, p, p), block, V2I(0, 1), d)
              bverts += BVert(offset + v + V3F(p, p, n), block, V2I(1, 1), d)
              bindices.append(Seq(2, 1, 0, 3, 2, 0).map(i => (i + bindex).toShort): _*)
              bindex += 4
            case East =>
              bverts += BVert(offset + v + V3F(n, n, n), block, V2I(0, 0), d)
              bverts += BVert(offset + v + V3F(n, n, p), block, V2I(1, 0), d)
              bverts += BVert(offset + v + V3F(n, p, p), block, V2I(1, 1), d)
              bverts += BVert(offset + v + V3F(n, p, n), block, V2I(0, 1), d)
              bindices.append(Seq(1, 2, 0, 2, 3, 0).map(i => (i + bindex).toShort): _*)
              bindex += 4
            case Up =>
              bverts += BVert(offset + v + V3F(n, p, n), block, V2I(1, 0), d)
              bverts += BVert(offset + v + V3F(p, p, n), block, V2I(0, 0), d)
              bverts += BVert(offset + v + V3F(p, p, p), block, V2I(0, 1), d)
              bverts += BVert(offset + v + V3F(n, p, p), block, V2I(1, 1), d)
              bindices.append(Seq(2, 1, 0, 3, 2, 0).map(i => (i + bindex).toShort): _*)
              bindex += 4
            case Down =>
              bverts += BVert(offset + v + V3F(n, n, n), block, V2I(1, 1), d)
              bverts += BVert(offset + v + V3F(p, n, n), block, V2I(0, 1), d)
              bverts += BVert(offset + v + V3F(p, n, p), block, V2I(0, 0), d)
              bverts += BVert(offset + v + V3F(n, n, p), block, V2I(1, 0), d)
              bindices.append(Seq(1, 2, 0, 2, 3, 0).map(i => (i + bindex).toShort): _*)
              bindex += 4
          }
        }
      }

      Some(CompleteTerrain(pos, grid, verts, indices, indexToVert, vertToIndex.immutabilize, bverts, bindices))
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
case class CompleteTerrain(pos: V3I, grid: IDField[TerrainUnit],
                           verts: OptionField[TVert], indices: Seq[NodeTypeID], indexToVert: Seq[V3I], vertToIndex: ShortField,
                           bverts: Seq[BVert], bindices: Seq[Short]
                          ) extends Terrain {
  override def isComplete: Boolean = true

  override def asComplete: Option[CompleteTerrain] = Some(this)
}
object CompleteTerrain {
  @CarboniteFields case class TVert(pos: V3F, mat: TerrainUnit)
  @CarboniteFields case class BVert(pos: V3F, block: Block, uvDelta: V2I, nor: V3F)
}