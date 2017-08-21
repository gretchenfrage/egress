package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.math.{Ones, Origin, V3F, V3I}

trait World {

  def chunkAt(p: V3I): Option[Chunk]

  def time: Long

  def res: Int

  def resVec = V3I(res, res, res)

  def findEntity(id: UUID): Option[Entity]

  def density(v: V3F): Option[Float] = {
    val g = v / res * 16

    val v0 = g floor
    val v1 = g ceil

    def point(v: V3I): Option[Float] =
      chunkAt(v / res floor).map(_.densities(v % res).get)

    if (v0 == v1) point(v0)
    else if (chunkAt(v0 / res floor).isDefined && chunkAt(v1 / res floor).isDefined) {
      val d = (g - v0) \\ (v1 - v0)

      val c00 = point(V3I(v0.xi, v0.yi, v0.zi)).get * (1 - d.x) + point(V3I(v1.xi, v0.yi, v0.zi)).get * d.x
      val c01 = point(V3I(v0.xi, v0.yi, v1.zi)).get * (1 - d.x) + point(V3I(v1.xi, v0.yi, v1.zi)).get * d.x
      val c10 = point(V3I(v0.xi, v1.yi, v0.zi)).get * (1 - d.x) + point(V3I(v1.xi, v1.yi, v0.zi)).get * d.x
      val c11 = point(V3I(v0.xi, v1.yi, v1.zi)).get * (1 - d.x) + point(V3I(v1.xi, v1.yi, v1.zi)).get * d.x

      val c0 = c00 * (1 - d.y) + c10 * d.y
      val c1 = c01 * (1 - d.y) + c11 * d.y

      Some(c0 * (1 - d.z) + c1 * d.z)
    } else None
  }

}
