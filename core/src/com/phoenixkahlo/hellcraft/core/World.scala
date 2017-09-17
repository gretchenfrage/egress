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

  def density(v: V3I): Option[Float] =
    chunkAt(v / res floor).map(_.terrain.densities.atMod(v))

  def sampleDensity(v: V3F): Option[Float] = {
    if (v % 1 == Origin) density(v.toInts)
    else {
      val v0 = v.floor.toInts
      val v1 = v0 + Ones
      if (v0.to(v1).map(_ / res floor).forall(chunkAt(_).isDefined)) {
        val d = (v - v0) \\ (v1 - v0)

        val c00 = density(V3I(v0.xi, v0.yi, v0.zi)).get * (1 - d.x) + density(V3I(v1.xi, v0.yi, v0.zi)).get * d.x
        val c01 = density(V3I(v0.xi, v0.yi, v1.zi)).get * (1 - d.x) + density(V3I(v1.xi, v0.yi, v1.zi)).get * d.x
        val c10 = density(V3I(v0.xi, v1.yi, v0.zi)).get * (1 - d.x) + density(V3I(v1.xi, v1.yi, v0.zi)).get * d.x
        val c11 = density(V3I(v0.xi, v1.yi, v1.zi)).get * (1 - d.x) + density(V3I(v1.xi, v1.yi, v1.zi)).get * d.x

        val c0 = c00 * (1 - d.y) + c10 * d.y
        val c1 = c01 * (1 - d.y) + c11 * d.y

        val c = c0 * (1 - d.z) + c1 * d.z

        Some(c)
      } else None
    }
  }

  def direction(v: V3I): Option[V3F] = {
    Directions().map(d => density(v + d).map(d * _)).fold(Some(Origin))({
      case (Some(a), Some(b)) => Some(a + b)
      case _ => None
    }).map(_ / 6).map(_.tryNormalize)
  }

  def sampleDirection(v: V3F): Option[V3F] = {
    if (v % 1 == Origin) direction(v.toInts)
    else {
      val v0 = v.floor.toInts
      val v1 = v0 + Ones
      if (v0.to(v1).map(_ / res floor).forall(chunkAt(_).isDefined)) {
        val d = (v - v0) \\ (v1 - v0)

        val c00 = direction(V3I(v0.xi, v0.yi, v0.zi)).get * (1 - d.x) + direction(V3I(v1.xi, v0.yi, v0.zi)).get * d.x
        val c01 = direction(V3I(v0.xi, v0.yi, v1.zi)).get * (1 - d.x) + direction(V3I(v1.xi, v0.yi, v1.zi)).get * d.x
        val c10 = direction(V3I(v0.xi, v1.yi, v0.zi)).get * (1 - d.x) + direction(V3I(v1.xi, v1.yi, v0.zi)).get * d.x
        val c11 = direction(V3I(v0.xi, v1.yi, v1.zi)).get * (1 - d.x) + direction(V3I(v1.xi, v1.yi, v1.zi)).get * d.x

        val c0 = c00 * (1 - d.y) + c10 * d.y
        val c1 = c01 * (1 - d.y) + c11 * d.y

        val c = c0 * (1 - d.z) + c1 * d.z

        Some(c.tryNormalize)
      } else None
    }
  }

}
