package com.phoenixkahlo.hellcraft.core.client

import com.phoenixkahlo.hellcraft.core.{Air, Chunk, Terrain, TerrainUnit}
import com.phoenixkahlo.hellcraft.core.entity.{EntID, Entity}
import com.phoenixkahlo.hellcraft.math._

case class WorldBounds(min: V3I, max: V3I) {
  def minMax: (V3I, V3I) = (min, max)
}

trait ClientWorld {
  def chunk(p: V3I): Option[Chunk]

  def terrain(p: V3I): Option[Terrain]

  def findEntity[E <: Entity[E]](id: EntID[E]): Option[E]

  def bounds: WorldBounds

  def loadedChunks: Iterable[V3I]

  def loadedTerrain: Iterable[V3I]

  def raycast(pos: V3F, dir: V3F): Stream[V3F] = {
    val (min, max) = bounds.minMax
    Raytrace.voxels(pos / 16, dir)
      .takeWhile(p => p > min && p < (max + Ones))
      .flatMap(chunk)
      .flatMap(chunk => {
        val tm: (Seq[Short], Short => V3F) =
          (chunk.terrainSoup.indices, i => chunk.terrainSoup.verts(chunk.terrainSoup.indexToVert(i)).get.pos)
        val bm: (Seq[Short], Short => V3F) =
          (chunk.blockSoup.indices, i => chunk.blockSoup.verts(i).pos)
        Raytrace.meshes(pos, dir, Seq(tm, bm))
      })
  }

  def rayhit(pos: V3F, dir: V3F): Option[V3F] =
    raycast(pos, dir).headOption

  def segcast(pos: V3F, dir: V3F, dist: Float): Stream[V3F] = {
    val (min, max) = bounds.minMax
    Raytrace.voxels(pos / 16, dir)
      .takeWhile(p => (p * 16).dist(pos) < (dist + 28)) // magic number 28 is a little greater than diagonal chunk size
      .takeWhile(p => p > min && p < (max + Ones))
      .flatMap(chunk)
      .flatMap(chunk => {
        val tm: (Seq[Short], Short => V3F) =
          (chunk.terrainSoup.indices, i => chunk.terrainSoup.verts(chunk.terrainSoup.indexToVert(i)).get.pos)
        val bm: (Seq[Short], Short => V3F) =
          (chunk.blockSoup.indices, i => chunk.blockSoup.verts(i).pos)
        Raytrace.meshes(pos, dir, Seq(tm, bm))
      })
      .takeWhile(_.dist(pos) <= dist)
  }

  def seghit(pos: V3F, dir: V3F, dist: Float): Option[V3F] =
    segcast(pos, dir, dist).headOption

  def terrainRay(pos: V3F, dir: V3F): Stream[(V3I, TerrainUnit)] = {
    val (min, max) = bounds.minMax
    Raytrace.voxels(pos, dir)
      .takeWhile(v => (v / 16) > min && (v / 16) < (max + Ones))
      .takeWhile(v => chunk(v / 16 floor) isDefined)
      .map(v => (v, voxel(v).get))
  }

  def placeBlock(pos: V3F, dir: V3F, dist: Float): Option[V3I] = {
    seghit(pos, dir, dist).flatMap(
      hit => terrainRay(hit + Repeated(0.5f), dir.neg).find({ case (_, t) => t == Air }).map({ case (v, _) => v }))
  }

  def placeMat(pos: V3F, dir: V3F, dist: Float): Option[V3I] = {
    seghit(pos, dir, dist).flatMap(
      hit => terrainRay(hit, dir.neg).find({ case (_, t) => t == Air }).map({ case (v, _) => v })
    )
  }

  def voxel(v: V3I): Option[TerrainUnit] =
    terrain(v / 16 floor).map(_.grid.atMod(v))

  def sampleDensity(vWorld: V3F): Option[Float] = {
    val vGrid = vWorld
    if (vGrid % 1 == Origin)
      voxel(vGrid toInts).map(mat => if (mat.id <= 0) 0f else 1f)
    else {
      // trilinear interpolation
      val v0 = vGrid.floor
      val v1 = v0 + Ones
      if (v0.toAsSeq(v1).map(_ / 16 floor).forall(terrain(_).isDefined)) {
        // helper function (stands for density grid point)
        def dgp(x: V3I, y: V3I, z: V3I): Float =
          if (voxel(V3I(x.xi, y.yi, z.zi)).get.id <= 0) 0 else 1

        val d = (vGrid - v0) \\ (v1 - v0)

        val c00 = dgp(v0, v0, v0) * (1 - d.x) + dgp(v1, v0, v0) * d.x
        val c01 = dgp(v0, v0, v1) * (1 - d.x) + dgp(v1, v0, v1) * d.x
        val c10 = dgp(v0, v1, v0) * (1 - d.x) + dgp(v1, v1, v0) * d.x
        val c11 = dgp(v0, v1, v1) * (1 - d.x) + dgp(v1, v1, v1) * d.x

        val c0 = c00 * (1 - d.y) + c10 * d.y
        val c1 = c01 * (1 - d.y) + c11 * d.y

        val c = c0 * (1 - d.z) + c1 * d.z

        Some(c)
      } else None
    }
  }

  def sampleDirection(v: V3F): Option[V3F] =
    Directions().map(d => sampleDensity(d * 0.01f + v).map(d * _)).fold(Some(Origin))({
      case (Some(a), Some(b)) => Some(a + b)
      case _ => None
    }).map(v => (v / 6).normalize)

}
