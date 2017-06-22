package com.phoenixkahlo.hellcraft.math

import com.phoenixkahlo.hellcraft.core.{Air, World}

object Raytrace {

  def collisions(v: V3I, pos: V3F, dir: V3F): Stream[V3I] = {
    if (dir.magnitude == 0) Nil

    val line = ParamLine3D(pos, dir)

    val xy: (XYPlane, V3I) =
      if (dir.z < 0) (XYPlane(v.z), v + South)
      else (XYPlane(v.z + 1), v + North)
    val yz: (YZPlane, V3I) =
      if (dir.x < 0) (YZPlane(v.x), v + West)
      else (YZPlane(v.x + 1), v + East)
    val xz: (XZPlane, V3I) =
      if (dir.y < 0) (XZPlane(v.y), v + Down)
      else (XZPlane(v.y + 1), v + Up)

    val (nextPos: V3F, nextV: V3I) =
      Seq(xy, yz, xz)
      .map({ case (plane, block) => (line intersection plane, block) })
      .sortBy({ case (point, _) => point dist pos })
      .head

    nextV #:: collisions(nextV, nextPos, dir)
  }

  def hit(pos: V3F, dir: V3F, world: World, range: Float = 4): Option[V3I] = {
    collisions(pos floor, pos, dir)
      .takeWhile(v => (v + Repeated(0.5f) dist pos) <= range)
      .takeWhile(v => world.chunkIsDefinedAt(v / 16 floor))
      .find(v => world.blockAt(v).get != Air)
  }

  def place(pos: V3F, dir: V3F, world: World, range: Float = 4): Option[V3I] = {
    collisions(pos floor, pos, dir)
      .takeWhile(v => (v + Repeated(0.5f) dist pos) <= range)
      .takeWhile(v => world.blockAt(v).contains(Air))
      .lastOption
  }

}
