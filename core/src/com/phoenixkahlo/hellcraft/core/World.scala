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

  /**
    * Uses density grid coordinates, not world coordinates!
    */
  def densityGridPoint(v: V3I): Option[Float] =
    chunkAt(v / res floor).map(_.terrain.densities.atMod(v))

  def materialGridPoint(v: V3I): Option[Material] =
    chunkAt(v / res floor).map(_.terrain.materials.atMod(v)).map(Materials(_))

  /**
    * Samples the density at an arbitrary point using trilinear interpolation.
    */
  def sampleDensity(vWorld: V3F): Option[Float] = {
    val vGrid = vWorld / 16f * res
    if (vGrid % 1 == Origin) densityGridPoint(vGrid.toInts)
    else {
      val v0 = vGrid.floor.toInts
      val v1 = v0 + Ones
      if (v0.to(v1).map(_ / res floor).forall(chunkAt(_).isDefined)) {
        // alias for the function
        val dgp: V3I => Option[Float] = densityGridPoint

        val d = (vGrid - v0) \\ (v1 - v0)

        val c00 = dgp(V3I(v0.xi, v0.yi, v0.zi)).get * (1 - d.x) + dgp(V3I(v1.xi, v0.yi, v0.zi)).get * d.x
        val c01 = dgp(V3I(v0.xi, v0.yi, v1.zi)).get * (1 - d.x) + dgp(V3I(v1.xi, v0.yi, v1.zi)).get * d.x
        val c10 = dgp(V3I(v0.xi, v1.yi, v0.zi)).get * (1 - d.x) + dgp(V3I(v1.xi, v1.yi, v0.zi)).get * d.x
        val c11 = dgp(V3I(v0.xi, v1.yi, v1.zi)).get * (1 - d.x) + dgp(V3I(v1.xi, v1.yi, v1.zi)).get * d.x

        val c0 = c00 * (1 - d.y) + c10 * d.y
        val c1 = c01 * (1 - d.y) + c11 * d.y

        val c = c0 * (1 - d.z) + c1 * d.z

        Some(c)
      } else None
    }
  }

  def direction(v: V3I): Option[V3F] = {
    Directions().map(d => densityGridPoint(v + d).map(d * _)).fold(Some(Origin))({
      case (Some(a), Some(b)) => Some(a + b)
      case _ => None
    }).map(_ / 6).map(_.tryNormalize)
  }

  /**
    * Samples the density field's direction at an arbitrary point.
    */
  def sampleDirection(v: V3F): Option[V3F] = {
    Directions().map(d => sampleDensity(d * 0.01f + v).map(d * _)).fold(Some(Origin))({
      case (Some(a), Some(b)) => Some(a + b)
      case _ => None
    }).map(_ / 6).map(_.tryNormalize)
  }

  def raycast(pos: V3F, dir: V3F): Stream[V3F] = {
    val (min, max) = boundingBox
    Raytrace.voxels(pos / 16, dir).takeWhile(v => v > min && v < max).flatMap(chunkAt).flatMap(_.terrain.asMeshable)
      .flatMap(meshable => {
        Raytrace.mesh(pos, dir, meshable.indices, i => meshable.vertices(meshable.vertMap(i)).get.p)
      })
  }

  def rayhit(pos: V3F, dir: V3F): Option[V3F] =
    raycast(pos, dir).headOption

}
