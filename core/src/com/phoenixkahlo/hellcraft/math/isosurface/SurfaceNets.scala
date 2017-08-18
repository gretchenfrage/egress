package com.phoenixkahlo.hellcraft.math.isosurface

import com.phoenixkahlo.hellcraft.core.World
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.math._

import scala.collection.mutable.ArrayBuffer

object SurfaceNets {

  val edges = Seq(
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



  def apply(size: V3I, density: V3F => Float): Seq[Quad] = {
    val grid = new Array[V3F](size.fold(_ * _))

    // create the grid of vertices, and assign them indices
    for (v <- grid.indices.map(size.decompress)) {
      val points = new ArrayBuffer[V3F]

      for ((v1, v2) <- edges.map({ case (v1, v2) => (v1 + v, v2 + v) })) {
        if ((density(v1) == 0) ^ (density(v2) == 0))
          points += ((v1 + v2) / 2)
      }

      if (points nonEmpty) {
        grid(size.compress(v)) = points.fold(Origin)(_ + _) / points.size
      }
    }

    // create the quads
    val quads = new ArrayBuffer[Quad]
    def maybeAddQuad(a: V3I, b: V3I, c: V3I, d: V3I): Unit = {
      val vertices: Seq[V3F] = Seq(a, b, c, d).map(v => grid(size.compress(v)))
      if (!vertices.contains(null)) {
        quads += Quad(vertices(0), vertices(1), vertices(2), vertices(3))
      }
    }
    // z axis
    for {
      x <- 0 until size.xi - 1
      y <- 0 until size.yi - 1
      z <- 0 until size.zi - 2
    } yield maybeAddQuad(V3I(x, y, z), V3I(x + 1, y, z), V3I(x + 1, y + 1, z), V3I(x, y + 1, z))
    // x axis
    for {
      x <- 0 until size.xi - 2
      y <- 0 until size.yi - 1
      z <- 0 until size.zi - 1
    } yield maybeAddQuad(V3I(x, y, z), V3I(x, y + 1, z), V3I(x, y + 1, z + 1), V3I(x, y, z + 1))
    // y axis
    for {
      x <- 0 until size.xi - 1
      y <- 0 until size.yi - 2
      z <- 0 until size.zi - 1
    } yield maybeAddQuad(V3I(x, y, z), V3I(x + 1, y, z), V3I(x + 1, y, z + 1), V3I(x, y, z + 1))

    quads
  }
  

}