package com.phoenixkahlo.hellcraft.math

import com.phoenixkahlo.hellcraft.core.World

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
        .map({ case (plane, block) => (line intersection plane, block) }).minBy { case (point, _) => point dist pos }

    nextV #:: collisions(nextV, nextPos, dir)
  }

}
