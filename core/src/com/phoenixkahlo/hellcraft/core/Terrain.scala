package com.phoenixkahlo.hellcraft.core

import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.caches.ParamCache
import com.phoenixkahlo.hellcraft.util.fields.OptionField

import scala.collection.mutable.ArrayBuffer

class Terrain(chunk: Chunk) {

  val vertices: ParamCache[World, OptionField[V3F]] = new ParamCache(world => {
    OptionField(world.resVec, i => {
      // real world coordinates of the middle of the cube
      val v = (i / world.res + chunk.pos) * 16

      val spoints = new ArrayBuffer[V3F]

      val h = 16f / world.res / 2f
      val edges: Seq[(V3F, V3F)] = Seq(
        (v + V3F(-h, -h, -h)) -> (v + V3F(h, -h, -h)),
        (v + V3F(-h, -h, -h)) -> (v + V3F(-h, h, -h)),
        (v + V3F(-h, -h, -h)) -> (v + V3F(-h, -h, h)),

        (v + V3F(-h, -h, h)) -> (v + V3F(-h, h, h)),
        (v + V3F(h, -h, -h)) -> (v + V3F(h, h, -h)),
        (v + V3F(h, -h, h)) -> (v + V3F(h, h, h)),

        (v + V3F(-h, h, -h)) -> (v + V3F(-h, h, h)),
        (v + V3F(-h, h, -h)) -> (v + V3F(h, h, -h)),

        (v + V3F(-h, -h, h)) -> (v + V3F(h, -h, h)),
        (v + V3F(h, -h, -h)) -> (v + V3F(h, -h, h)),

        (v + V3F(-h, h, h)) -> (v + V3F(h, h, h)),
        (v + V3F(h, h, -h)) -> (v + V3F(h, h, h))
      )

      for ((v1, v2) <- edges) {
        if ((world.density(v1).get > 0.5f) != (world.density(v2).get > 0.5f)) {
          spoints += ((v1 + v2) / 2)
        }
      }
      if (spoints isEmpty) None
      else Some(spoints.fold(Origin)(_ + _) / spoints.size)
    })
  })

  val quads: ParamCache[World, Seq[Quad]] = new ParamCache(world => {
    val dim = world.resVec.dim.get

    def vert(v: V3I): Option[V3F] = {
      if (v >= Origin && v < world.resVec) vertices(world)(v)
      else {
        val global = v + (chunk.pos * dim)
        world.chunkAt(global / dim floor).get.terrain.vertices(world)(global % dim)
      }
    }

    Origin.until(world.resVec)
      .flatMap(v => Seq(
        (v, v + North, v + North + East, v + East),
        (v, v + Up, v + Up + East, v + East),
        (v, v + Up, v + Up + North, v + North)
      ))
      .map({ case (v1, v2, v3, v4) => (vert(v1), vert(v2), vert(v3), vert(v4)) })
      .flatMap({
        case (Some(a), Some(b), Some(c), Some(d)) => Some(Quad(a, b, c, d))
        case _ => None
      })
  })

}