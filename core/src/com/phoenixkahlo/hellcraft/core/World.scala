package com.phoenixkahlo.hellcraft.core

import java.util.{NoSuchElementException, UUID}

import com.phoenixkahlo.hellcraft.core.entity.{Entity, EntID}
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.debugging.Profiler




//trait World {
  //def chunkAt(p: V3I): Option[Chunk]

  //def time: Long

  //def findEntity[E <: Entity[E]](id: EntID[E]): Option[E]

  //def boundingBox: (V3I, V3I)

  //def debugLoadedChunks: Iterable[V3I]

  //def debugLoadedTerrain: Iterable[V3I]

  /*
  def raycast(pos: V3F, dir: V3F): Stream[V3F] = {
    val (min, max) = boundingBox
    Raytrace.voxels(pos / 16, dir)
      .takeWhile(p => p > min && p < (max + Ones))
      .flatMap(chunkAt)
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
    val (min, max) = boundingBox
    Raytrace.voxels(pos / 16, dir)
      .takeWhile(p => (p * 16).dist(pos) < (dist + 28)) // magic number 28 is a little greater than diagonal chunk size
      .takeWhile(p => p > min && p < (max + Ones))
      .flatMap(chunkAt)
      .flatMap(chunk => {
        val tm: (Seq[Short], Short => V3F) =
          (chunk.terrainSoup.indices, i => chunk.terrainSoup.verts(chunk.terrainSoup.indexToVert(i)).get.pos)
        val bm: (Seq[Short], Short => V3F) =
          (chunk.blockSoup.indices, i => chunk.blockSoup.verts(i).pos)
        /*
        val tm: Option[(Seq[Short], Short => V3F)] =
          chunk.terrainSoup.map(soup => (soup.indices, i => soup.verts(soup.indexToVert(i)).get.pos))
        val bm: Option[(Seq[Short], Short => V3F)] =
          chunk.blockSoup.map(soup => (soup.indices, i => soup.verts(i).pos))
          */
        Raytrace.meshes(pos, dir, Seq(tm, bm))
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
  */
//}
