package com.phoenixkahlo.hellcraft.core

import java.util.{NoSuchElementException, UUID}

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.math.{Ones, Origin, V3F, V3I}
import com.phoenixkahlo.hellcraft.util.debugging.Profiler

trait World {

  def chunkAt(p: V3I): Option[Chunk]

  def time: Long

  def res: Int

  def resVec = V3I(res, res, res)

  def findEntity(id: UUID): Option[Entity]

  def density(v: V3I): Option[Float] =
    chunkAt(v / res floor).map(_.terrain.densities.atMod(v))

  /**
    * Samples the density using trilinear interpolation
    */
  /*
  def density(v: V3F): Option[Float] = {
    val p = Profiler("world density sample")

    val g = v / res * 16

    val v0 = g floor
    val v1 = g ceil

    p.log()

    def point(v: V3I): Option[Float] =
      chunkAt(v / res floor).map(_.terrain.densities(v % res).get)

    if (v0 == v1) point(v0)
    else try {
      p.log()

      val d = (g - v0) \\ (v1 - v0)

      p.log()

      val d000 = chunkAt(v0 / res floor).get.terrain.densities
      val d100 = chunkAt(V3I(v1.xi, v0.yi, v0.zi) / res floor).get.terrain.densities
      val d010 = chunkAt(V3I(v0.xi, v1.yi, v0.zi) / res floor).get.terrain.densities
      val d001 = chunkAt(V3I(v0.xi, v0.yi, v1.zi) / res floor).get.terrain.densities
      val d011 = chunkAt(V3I(v0.xi, v1.yi, v1.zi) / res floor).get.terrain.densities
      val d101 = chunkAt(V3I(v1.xi, v0.yi, v1.zi) / res floor).get.terrain.densities
      val d110 = chunkAt(V3I(v1.xi, v1.yi, v0.zi) / res floor).get.terrain.densities
      val d111 = chunkAt(v1 / res floor).get.terrain.densities

      p.log()

      val dx = d.x
      val omdx = 1 - dx

      val c00 = d000.atMod(v0) * omdx + d100.atMod(V3I(v1.xi, v0.yi, v0.zi)) * dx
      val c01 = d001.atMod(V3I(v0.xi, v0.yi, v1.zi)) * omdx + d101.atMod(V3I(v1.xi, v0.yi, v1.zi)) * dx
      val c10 = d010.atMod(V3I(v0.xi, v1.yi, v0.zi)) * omdx + d110.atMod(V3I(v1.xi, v0.yi, v1.zi)) * dx
      val c11 = d011.atMod(V3I(v0.xi, v1.yi, v1.zi)) * omdx + d111.atMod(v1) * dx


      p.log()

      val c0 = c00 * (1 - d.y) + c10 * d.y
      val c1 = c01 * (1 - d.y) + c11 * d.y

      p.log()

      val c = c0 * (1 - d.z) + c1 * d.z

      p.log()
      p.printDisc(1)

      Some(c)
    } catch {
      case e: NoSuchElementException => None
    }
    /*
    if (v0 == v1) point(v0)
    else if ((v0 / res floor).to(v1 / res floor).forall(chunkAt(_).isDefined)) {
      p.log()

      val d = (g - v0) \\ (v1 - v0)

      p.log()

      /*
      val c00 = point(V3I(v0.xi, v0.yi, v0.zi)).get * (1 - d.x) + point(V3I(v1.xi, v0.yi, v0.zi)).get * d.x
      val c01 = point(V3I(v0.xi, v0.yi, v1.zi)).get * (1 - d.x) + point(V3I(v1.xi, v0.yi, v1.zi)).get * d.x
      val c10 = point(V3I(v0.xi, v1.yi, v0.zi)).get * (1 - d.x) + point(V3I(v1.xi, v1.yi, v0.zi)).get * d.x
      val c11 = point(V3I(v0.xi, v1.yi, v1.zi)).get * (1 - d.x) + point(V3I(v1.xi, v1.yi, v1.zi)).get * d.x
      */
      val d000 = chunkAt(v0 / res floor).get.terrain.densities
      val d100 = chunkAt(V3I(v1.xi, v0.yi, v0.zi) / res floor).get.terrain.densities
      val d010 = chunkAt(V3I(v0.xi, v1.yi, v0.zi) / res floor).get.terrain.densities
      val d001 = chunkAt(V3I(v0.xi, v0.yi, v1.zi) / res floor).get.terrain.densities
      val d011 = chunkAt(V3I(v0.xi, v1.yi, v1.zi) / res floor).get.terrain.densities
      val d101 = chunkAt(V3I(v1.xi, v0.yi, v1.zi) / res floor).get.terrain.densities
      val d110 = chunkAt(V3I(v1.xi, v1.yi, v0.zi) / res floor).get.terrain.densities
      val d111 = chunkAt(v1 / res floor).get.terrain.densities

      p.log()

      val dx = d.x
      val omdx = 1 - dx

      val c00 = d000.atMod(v0) * omdx + d100.atMod(V3I(v1.xi, v0.yi, v0.zi)) * dx
      val c01 = d001.atMod(V3I(v0.xi, v0.yi, v1.zi)) * omdx + d101.atMod(V3I(v1.xi, v0.yi, v1.zi)) * dx
      val c10 = d010.atMod(V3I(v0.xi, v1.yi, v0.zi)) * omdx + d110.atMod(V3I(v1.xi, v0.yi, v1.zi)) * dx
      val c11 = d011.atMod(V3I(v0.xi, v1.yi, v1.zi)) * omdx + d111.atMod(v1) * dx


      p.log()

      val c0 = c00 * (1 - d.y) + c10 * d.y
      val c1 = c01 * (1 - d.y) + c11 * d.y

      p.log()

      val c = c0 * (1 - d.z) + c1 * d.z

      p.log()
      p.printDisc(1)

      Some(c)
    } else None
    */
  }
  */

}
