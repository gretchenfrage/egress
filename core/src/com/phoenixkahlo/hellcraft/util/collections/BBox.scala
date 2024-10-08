package com.phoenixkahlo.hellcraft.util.collections

import com.phoenixkahlo.hellcraft.core.client.WorldBounds
import com.phoenixkahlo.hellcraft.math.{Origin, V3I}

import scala.collection.immutable.SortedMap

case class BBox(x: SortedMap[Int, Int], y: SortedMap[Int, Int], z: SortedMap[Int, Int], v: Set[V3I]) {
  //def apply() =
  //  if (v.nonEmpty) (V3I(x.min._1, y.min._1, z.min._1), V3I(x.max._1, y.max._1, z.max._1))
  //  else (Origin, Origin)
  def apply(): WorldBounds =
    if (v.nonEmpty) WorldBounds(V3I(x.min._1, y.min._1, z.min._1), V3I(x.max._1, y.max._1, z.max._1))
    else WorldBounds(Origin, Origin)

  def +(p: V3I): BBox = {
    if (v(p)) this
    else {
      BBox(
        x + ((p.xi, x.getOrElse(p.xi, 0) + 1)),
        y + ((p.yi, y.getOrElse(p.yi, 0) + 1)),
        z + ((p.zi, z.getOrElse(p.zi, 0) + 1)),
        v + p
      )
    }
  }

  def -(p: V3I): BBox = {
    if (!v(p)) this
    else {
      val subbed = BBox(
        x + ((p.xi, x(p.xi) - 1)),
        y + ((p.yi, y(p.yi) - 1)),
        z + ((p.zi, z(p.zi) - 1)),
        v - p
      )
      BBox(
        if (subbed.x(p.xi) > 0) subbed.x else subbed.x - p.xi,
        if (subbed.y(p.yi) > 0) subbed.y else subbed.y - p.yi,
        if (subbed.z(p.zi) > 0) subbed.z else subbed.z - p.zi,
        subbed.v
      )
    }
  }

  def ++(ps: Traversable[V3I]): BBox =
    ps.foldLeft(this)((a, p) => a + p)

  def --(ps: Traversable[V3I]): BBox =
    ps.foldLeft(this)((a, p) => a - p)
}

object BBox {
  val empty = BBox(SortedMap.empty, SortedMap.empty, SortedMap.empty, Set.empty)
}
