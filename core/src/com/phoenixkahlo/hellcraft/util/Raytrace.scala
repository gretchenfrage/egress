package com.phoenixkahlo.hellcraft.util

import com.phoenixkahlo.hellcraft.{Air, World}

object Raytrace {

  def apply(pos: V3F, dir: V3F): Stream[V3I] = {
    if (dir.magnitude == 0) Nil

    val line = ParamLine3D(pos, dir)

    val xy: (XYPlane, V3I) =
      if (dir.z < 0) (XYPlane(pos.z.toInt), pos.floor + South)
      else (XYPlane(pos.z.toInt + 1), pos.floor + North)
    val yz: (YZPlane, V3I) =
      if (dir.x < 0) (YZPlane(pos.x.toInt), pos.floor + West)
      else (YZPlane(pos.x.toInt + 1), pos.floor + East)
    val xz: (XZPlane, V3I) =
      if (dir.y < 0) (XZPlane(pos.y.toInt), pos.floor + Down)
      else (XZPlane(pos.y.toInt + 1), pos.floor + Up)

    Seq(xy, yz, xz)
      .map({ case (plane, block) => (line intersection plane, block) })
      .sortBy({ case (point, _) => point dist pos })
      .head match {
      case (point, block) => Stream.cons(block, apply(point + (dir.normalize * 0.0001f), dir))
    }
  }

  def hit(pos: V3F, dir: V3F, world: World): Option[V3I] =
    apply(pos, dir).takeWhile(world.blockAt(_) isDefined).find(world.blockAt(_).get != Air)

  def place(pos: V3F, dir: V3F, world: World): Option[V3I] =
    apply(pos, dir).takeWhile(world.blockAt(_).exists(_ == Air)).lastOption

}
