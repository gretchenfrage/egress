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

  case class Vertex(i: Int, p: V3F)

  def compute(start: V3I, end: V3I, density: V3F => Float): Seq[Quad] = {
    val size = end - start
    var i = 0
    val grid = new Array[Vertex](size.fold(_ * _))

    // create the grid of vertices, and assign them indices
    for (v <- grid.indices.map(size.decompress)) {
      val points = new ArrayBuffer[V3F]

      for ((v1, v2) <- edges.map({ case (v1, v2) => (v1 + v, v2 + v) })) {
        if ((density(v1) == 0) ^ (density(v2) == 0))
          points += ((v1 + v2) / 2)
      }

      if (points nonEmpty) {
        grid(size.compress(v)) = Vertex(i, points.fold(Origin)(_ + _) / points.size)
        i += 1
      }
    }

    // create the quads
    val quads = new ArrayBuffer[Quad]
    def maybeAddQuad(a: V3I, b: V3I, c: V3I, d: V3I): Unit = {
      val vertices: Seq[Vertex] = Seq(a, b, c, d).map(v => grid(size.compress(v)))
      if (!vertices.contains(null)) {
        quads += Quad(vertices(0).p, vertices(1).p, vertices(2).p, vertices(3).p)
      }
    }
    // z axis
    for {
      x <- start.xi until end.xi
      y <- start.yi until end.yi
      z <- start.zi to end.zi
    } yield maybeAddQuad(V3I(x, y, z), V3I(x + 1, y, z), V3I(x + 1, y + 1, z), V3I(x, y + 1, z))
    // x axis
    for {
      x <- start.xi to end.xi
      y <- start.yi until end.yi
      z <- start.zi until end.zi
    } yield maybeAddQuad(V3I(x, y, z), V3I(x, y + 1, z), V3I(x, y + 1, z + 1), V3I(x, y, z + 1))
    // y axis
    for {
      x <- start.xi until end.xi
      y <- start.yi to end.yi
      z <- start.zi until end.zi
    } yield maybeAddQuad(V3I(x, y, z), V3I(x + 1, y, z), V3I(x + 1, y, z + 1), V3I(x, y, z + 1))

    quads
  }
  

}