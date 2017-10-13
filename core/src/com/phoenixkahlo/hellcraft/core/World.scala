package com.phoenixkahlo.hellcraft.core

import java.util.{NoSuchElementException, UUID}

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.debugging.Profiler

trait World {

  def chunkAt(p: V3I): Option[Chunk]

  def time: Long

  def res: Int

  def resVec = V3I(res, res, res)

  def findEntity(id: UUID): Option[Entity]

  def boundingBox: (V3I, V3I)

  def terrainGridPoint(v: V3I): Option[TerrainUnit] =
    chunkAt(v / res floor).map(_.terrain.grid.atMod(v))

  def sampleDensity(vWorld: V3F): Option[Float] = {
    val vGrid = vWorld / 16f * res
    if (vGrid % 1 == Origin)
      terrainGridPoint(vGrid toInts).map(mat => if (mat.id <= 0) 0f else 1f)
    else {
      // trilinear interpolation
      val v0 = vGrid.floor
      val v1 = v0 + Ones
      if (v0.to(v1).map(_ / res floor).forall(chunkAt(_).isDefined)) {
        // helper function (stands for density grid point)
        def dgp(x: V3I, y: V3I, z: V3I): Float =
          if (terrainGridPoint(V3I(x.xi, y.yi, z.zi)).get.id <= 0) 0 else 1

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
    }).map(v => (v / 6).tryNormalize)

  def raycast(pos: V3F, dir: V3F): Stream[V3F] = {
    val (min, max) = boundingBox
    Raytrace.voxels(pos / 16, dir)
      .takeWhile(p => p > min && p < (max + Ones))
      .flatMap(chunkAt)
      .flatMap(chunk => {
        val tm: Option[(Seq[Short], Short => V3F)] =
          chunk.terrainSoup.map(soup => (soup.indices, i => soup.verts(soup.indexToVert(i)).get.pos))
        val bm: Option[(Seq[Short], Short => V3F)] =
          chunk.blockSoup.map(soup => (soup.indices, i => soup.verts(i).pos))
        Raytrace.meshes(pos, dir, Seq(tm, bm).flatten)
      })
  }

  def rayhit(pos: V3F, dir: V3F): Option[V3F] =
    raycast(pos, dir).headOption

  def segcast(pos: V3F, dir: V3F, dist: Float): Stream[V3F] = {
    val (min, max) = boundingBox
    Raytrace.voxels(pos / 16, dir)
      .takeWhile(p => (p * 16).dist(pos) < (dist + 28)) // magic number 28 is a little greater than diagonal chunk size
      .takeWhile(p => p > min && p < (max + Ones))
      .flatMap(chunkAt)
      .flatMap(chunk => {
        val tm: Option[(Seq[Short], Short => V3F)] =
          chunk.terrainSoup.map(soup => (soup.indices, i => soup.verts(soup.indexToVert(i)).get.pos))
        val bm: Option[(Seq[Short], Short => V3F)] =
          chunk.blockSoup.map(soup => (soup.indices, i => soup.verts(i).pos))
        Raytrace.meshes(pos, dir, Seq(tm, bm).flatten)
      })
      .takeWhile(_.dist(pos) <= dist)
  }

  def seghit(pos: V3F, dir: V3F, dist: Float): Option[V3F] =
    segcast(pos, dir, dist).headOption

  def terrainRay(pos: V3F, dir: V3F): Stream[(V3I, TerrainUnit)] = {
    val (min, max) = boundingBox
    Raytrace.voxels(pos / 16 * res, dir)
      .takeWhile(v => (v / res) > min && (v / res) < (max + Ones))
      .takeWhile(v => chunkAt(v / res floor) isDefined)
      .map(v => (v, terrainGridPoint(v).get))
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

  /*
  def raycast(pos: V3F, dir: V3F): Stream[V3F] = {
    val (min, max) = boundingBox
    Raytrace.voxels(pos / 16, dir)
      .takeWhile(p => p > min && p < (max + Ones))
      .flatMap(chunkAt)
      .map[Seq[(Seq[Short], Short => V3F)]](c => Seq(
        c.terrainSoup.map[(Seq[Short], Short => V3F)](soup => (soup.indices, i => soup.verts(soup.indexToVert(i)).get.pos)),
        c.blockSoup.map[(Seq[Short], Short => V3F)](soup => (soup.indices, i => soup.verts(i).pos))
      ).flatten)
      .flatMap(meshes => Raytrace.meshes(pos, dir, meshes: _*))
      /*
      .flatMap(terrain => Raytrace.meshes(pos, dir, Seq(
        (terrain.indices, i => terrain.verts(terrain.indexToVert(i)).get.pos),
        (terrain.bindices, i => terrain.bverts(i).pos)
      )))
      */
  }

  def rayhit(pos: V3F, dir: V3F): Option[V3F] =
    raycast(pos, dir).headOption

  def segcast(pos: V3F, dir: V3F, dist: Float): Stream[V3F] = {
    val (min, max) = boundingBox
    Raytrace.voxels(pos / 16, dir)
      .takeWhile(p => (p * 16).dist(pos) < (dist + 28)) // magic number 28 is a little greater than diagonal chunk size
      .takeWhile(p => p > min && p < (max + Ones))
      .flatMap(chunkAt)
      .flatMap(_.terrain.asComplete)
      .flatMap(terrain => Raytrace.meshes(pos, dir, Seq(
        (terrain.indices, i => terrain.verts(terrain.indexToVert(i)).get.pos),
        (terrain.bindices, i => terrain.bverts(i).pos)
      )))
      .takeWhile(_.dist(pos) <= dist)
  }

  def seghit(pos: V3F, dir: V3F, dist: Float): Option[V3F] =
    segcast(pos, dir, dist).headOption

  /*
  def terrainRay(pos: V3F, dir: V3F): Stream[(V3I, TerrainUnit)] = {
    val (min, max) = boundingBox
    Raytrace.voxels(pos, dir)
      .takeWhile(v => (v / 16) > min && (v / 16) < (max + Ones))
      .takeWhile(v => chunkAt(v / 16 floor) isDefined)
      .map(v => (v, terrainGridPoint(v).get))
  }

  def terrainSeg(pos: V3F, dir: V3F, dist: Float): Stream[(V3I, TerrainUnit)] =
    terrainRay(pos, dir).takeWhile({ case (v, _) => v.dist(pos) <= dist })

  def placeMat(pos: V3F, dir: V3F, dist: Float): Option[V3I] =
    terrainSeg(pos, dir, dist).takeWhile({ case (_, mat) => mat == Air }).lastOption.map({ case (v, _) => v })

  def placeBlock(pos: V3F, dir: V3F, dist: Float): Option[V3I] =
    placeMat(pos + V3F(0.5f, 0.5f, 0.5f), dir, dist)
    */

  def terrainRay(pos: V3F, dir: V3F): Stream[(V3I, TerrainUnit)] = {
    val (min, max) = boundingBox
    Raytrace.voxels(pos / 16 * res, dir)
      .takeWhile(v => (v / res) > min && (v / res) < (max + Ones))
      .takeWhile(v => chunkAt(v / res floor) isDefined)
      .map(v => (v, terrainGridPoint(v).get))
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

*/


}
