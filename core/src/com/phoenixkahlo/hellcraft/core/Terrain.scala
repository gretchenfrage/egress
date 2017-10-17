package com.phoenixkahlo.hellcraft.core

import com.phoenixkahlo.hellcraft.carbonite
import com.phoenixkahlo.hellcraft.carbonite._
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.BlockSoup.Vert
import com.phoenixkahlo.hellcraft.core.TerrainSoup.Vert
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.caches.ParamCache
import com.phoenixkahlo.hellcraft.util.debugging.Profiler
import com.phoenixkahlo.hellcraft.util.fields._

import scala.collection.mutable.ArrayBuffer

@CarboniteFields
case class Terrain(pos: V3I, grid: IDField[TerrainUnit])
object Terrain {
  def canComplete(pos: V3I, world: World): Boolean = pos.neighbors.forall(world.chunkAt(_).isDefined)
}

@CarboniteFields
case class TerrainSoup(pos: V3I, verts: OptionField[TerrainSoup.Vert], indices: Seq[Short], indexToVert: Seq[V3I],
                       vertToIndex: ShortField) extends Iterable[(V3F, V3F, V3F)] {
  override def iterator: Iterator[(V3F, V3F, V3F)] =
    Range(0, indices.size, 3).iterator
      .map(i => (
        verts(indexToVert(indices(i + 0))).get.pos,
        verts(indexToVert(indices(i + 1))).get.pos,
        verts(indexToVert(indices(i + 2))).get.pos
      ))
}
object TerrainSoup {
  @CarboniteFields case class Vert(pos: V3F, mat: Material, nor: V3F)

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

  def apply(terrain: Terrain, world: World): Option[TerrainSoup] =
  if (Terrain.canComplete(terrain.pos, world)) {
    val offset = terrain.pos * world.res

    // generate the vertex field with an overshoot of <1, 1, 1> to connect the chunks
    val verts = OptionField[TerrainSoup.Vert](world.resVec + Ones, v => {
      val corners = (v to v + Ones).map(v => world.terrainGridPoint(offset + v).get)
      if (corners.exists(_.id < 0)) {
        // if one of the corners is a block, we must snap to the center
        // but if none of the corners are terrain, we shouldn't place a vertex at all
        corners.find(_.id > 0).map(mat => {
          val pos = (offset + v + V3F(0.5f, 0.5f, 0.5f)) / world.res * 16
          Vert(pos, mat.asInstanceOf[Material], world.sampleDirection(pos).get.neg)
        })
      } else {
        // otherwise, we can do surface nets as normal

        // pairs of grid coords to test
        val edges = TerrainSoup.edges map { case (d1, d2) => (offset + v + d1, offset + v + d2) }
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
          // find a material that's not air
          val mat = (v to v + Ones).toStream
            .map(vv => world.terrainGridPoint(offset + vv).get).filterNot(_ == Air).head

          Some(TerrainSoup.Vert(vertPos, mat.asInstanceOf[Material], world.sampleDirection(vertPos).get.neg))
        }
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
      (d1, d2, d3) <- deltas
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
    Some(new TerrainSoup(terrain.pos, verts, indices, indexToVert, vertToIndex.immutabilize))
  } else None
}

@CarboniteFields
case class BlockSoup(pos: V3I, verts: Seq[BlockSoup.Vert], indices: Seq[NodeTypeID])
object BlockSoup {
  @CarboniteFields case class Vert(pos: V3F, block: Block, uvDelta: V2I, nor: V3F)

  def apply(terrain: Terrain, world: World): Option[BlockSoup] =
    if (Terrain.canComplete(terrain.pos, world)) {
      val offset = terrain.pos * world.res
      val grid = terrain.grid

      val n = -0.5f
      val p = 0.5f

      val verts = new ArrayBuffer[Vert]
      val indices = new ArrayBuffer[Short]
      var index = 0
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
          def mesh(x1: Float, y1: Float, z1: Float, u1: Int, v1: Int,
                   x2: Float, y2: Float, z2: Float, u2: Int, v2: Int,
                   x3: Float, y3: Float, z3: Float, u3: Int, v3: Int,
                   x4: Float, y4: Float, z4: Float, u4: Int, v4: Int,
                   i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int): Unit = {
            verts += Vert((offset + v + V3F(x1, y1, z1)) / world.res * 16, block, V2I(u1, v1), d)
            verts += Vert((offset + v + V3F(x2, y2, z2)) / world.res * 16, block, V2I(u2, v2), d)
            verts += Vert((offset + v + V3F(x3, y3, z3)) / world.res * 16, block, V2I(u3, v3), d)
            verts += Vert((offset + v + V3F(x4, y4, z4)) / world.res * 16, block, V2I(u4, v4), d)
            indices.append((index + i1).toShort, (index + i2).toShort, (index + i3).toShort)
            indices.append((index + i4).toShort, (index + i5).toShort, (index + i6).toShort)
            index += 4
          }
          d match {
            case South => mesh(
              n, n, n,    1, 0,
              p, n, n,    0, 0,
              p, p, n,    0, 1,
              n, p, n,    1, 1,
              2, 1, 0, 3, 2, 0
            )
            case North => mesh(
              n, n, p,    0, 0,
              p, n, p,    1, 0,
              p, p, p,    1, 1,
              n, p, p,    0, 1,
              1, 2, 0, 2, 3, 0
            )
            case West => mesh(
              p, n, n,    1, 0,
              p, n, p,    0, 0,
              p, p, p,    0, 1,
              p, p, n,    1, 1,
              2, 1, 0, 3, 2, 0
            )
            case East => mesh(
              n, n, n,    0, 0,
              n, n, p,    1, 0,
              n, p, p,    1, 1,
              n, p, n,    0, 1,
              1, 2, 0, 2, 3, 0
            )
            case Up => mesh(
              n, p, n,    1, 0,
              p, p, n,    0, 0,
              p, p, p,    0, 1,
              n, p, p,    1, 1,
              2, 1, 0, 3, 2, 0
            )
            case Down => mesh(
              n, n, n,    1, 1,
              p, n, n,    0, 1,
              p, n, p,    0, 0,
              n, n, p,    1, 0,
              1, 2, 0, 2, 3, 0
            )
          }
        }
      }
      Some(new BlockSoup(terrain.pos, verts, indices))
    } else None
}
