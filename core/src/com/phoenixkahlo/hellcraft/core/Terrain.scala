package com.phoenixkahlo.hellcraft.core

import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.ParamCache

import scala.collection.mutable.ArrayBuffer

class Terrain(chunk: Chunk) {

  val vertices: ParamCache[World, VectorField] = new ParamCache(world => {
    // field index vector + offset = local chunk space
    val dim = world.res.dim.get
    val offset = -(16 / dim / 2)

    // sample the density value of the local terrain coordinates
    def query(x: Int, y: Int, z: Int): Float = {
      val v = V3I(x, y, z)
      val b = chunk.densities(v).getOrElse({
        val global = v + (chunk.pos * dim)
        val p = global / dim floor
        val c = world.chunkAt(p).get
        c.densities(global % dim).get
      })
      b / 255f
    }

    // interpolate the density value of the local coordinates
    def sample(v: V3F): Float = {
      // algorithm taken from the wikipedia page on trilinear interpolation
      val i0: V3I = v / 16 * dim floor
      val i1: V3I = v / 16 * dim ceil
      val v0: V3F = i0 / dim * 16
      val v1: V3F = i1 / dim * 16

      val d: V3F = (v - v0) \\ (v1 - v0)

      val c00 = query(i0.xi, i0.yi, i0.zi) * (1 - d.x) + query(i1.xi, i0.yi, i0.zi) * d.x
      val c01 = query(i0.xi, i0.yi, i1.zi) * (1 - d.x) + query(i1.xi, i0.yi, i1.zi) * d.x
      val c10 = query(i0.xi, i1.yi, i0.zi) * (1 - d.x) + query(i1.xi, i1.yi, i0.zi) * d.x
      val c11 = query(i0.xi, i1.yi, i1.zi) * (1 - d.x) + query(i1.xi, i1.yi, i1.zi) * d.x

      val c0 = c00 * (1 - d.y) + c10 * d.y
      val c1 = c01 * (1 - d.y) + c11 * d.y

      c0 * (1 - d.z) + c1 * d.z
    }

    VectorField(world.res, i => {
      // convert the field index vector to the position in local chunk space
      //val v = i + Repeated(offset)

      val points = new ArrayBuffer[V3F]
      for ((i1, i2) <- Terrain.edges.map({ case (d1, d2) => (d1 + i, d2 + i) })) {
        if (sample())
      }



      ???
    })
  })

}

object Terrain {

  val edges: Seq[(V3I, V3I)] = Seq(
    (Origin, Up),
    (Origin, North),
    (Origin, East),
    (Ones, Ones + Down),
    (Ones, Ones + South),
    (Ones, Ones + West),
    (Up, Up + North),
    (Up, Up + East),
    (North, North + Up),
    (North, North + East),
    (East, East + Up),
    (East, East + North)
  )

  val point = 0.5f

}