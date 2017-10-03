package com.phoenixkahlo.hellcraft.math

import com.phoenixkahlo.hellcraft.core.World

import scala.collection.mutable.ArrayBuffer

object Raytrace {

  def voxels(v: V3I, pos: V3F, dir: V3F): Stream[V3I] = {
    if (dir.magnitude == 0) Nil

    val line = ParamLine3D(pos, dir)

    val xy: (XYPlane, V3I) =
      if (dir.z < 0) (XYPlane(v.z), v + South)
      else (XYPlane(v.z + 1), v + North)
    val yz: (YZPlane, V3I) =
      if (dir.x < 0) (YZPlane(v.x), v + East)
      else (YZPlane(v.x + 1), v + West)
    val xz: (XZPlane, V3I) =
      if (dir.y < 0) (XZPlane(v.y), v + Down)
      else (XZPlane(v.y + 1), v + Up)

    val (nextPos: V3F, nextV: V3I) =
      Seq(xy, yz, xz)
        .map({ case (plane, block) => (line intersection plane, block) }).minBy { case (point, _) => point dist pos }

    nextV #:: voxels(nextV, nextPos, dir)
  }

  /**
    * The Möller–Trumbore ray-triangle intersection algorithm
    */
  def triangle(src: V3F, dir: V3F, vert0: V3F, vert1: V3F, vert2: V3F): Option[V3F] = {
    val EPSILON = 1E-7f
    val edge1 = vert1 - vert0
    val edge2 = vert2 - vert0
    val h = dir cross edge2
    val a = edge1 dot h
    if (a > -EPSILON && a < EPSILON)
      return None
    val f = 1 / a
    val s = src - vert0
    val u = f * (s dot h)
    if (u < 0 || u > 1)
      return None
    val q = s cross edge1
    val v = f * (dir dot q)
    if (v < 0 || u + v > 1)
      return None
    val t = f * (edge2 dot q)
    if (t > EPSILON)
      Some(src + (dir.normalize * (t * dir.magnitude)))
    else
      None
  }

  /**
    * Intersect a ray with a mesh
    */
  def mesh(src: V3F, dir: V3F, indices: Seq[Short], vertices: Short => V3F): Option[V3F] = {
    val hits = new ArrayBuffer[V3F]
    for (i <- Range(0, indices.length, 3)) {
      triangle(src, dir,
        vertices(indices(i + 0)),
        vertices(indices(i + 1)),
        vertices(indices(i + 2))
      ).foreach(hits += _)
    }
    hits.sortBy(_ dist src).headOption
  }

  def meshes(src: V3F, dir: V3F, meshes: Seq[(Seq[Short], Short => V3F)]): Option[V3F] = {
    val hits = new ArrayBuffer[V3F]
    for {
      (indices, vertices) <- meshes
      i <- Range(0, indices.length, 3)
    } yield {
      triangle(src, dir,
        vertices(indices(i + 0)),
        vertices(indices(i + 1)),
        vertices(indices(i + 2))
      ).foreach(hits += _)
    }
    hits.sortBy(_ dist src).headOption
  }

}
