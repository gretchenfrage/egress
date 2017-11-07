package com.phoenixkahlo.hellcraft.math.physics

import com.phoenixkahlo.hellcraft.math.V3F

case class Triangle(p1: V3F, p2: V3F, p3: V3F) {
  val plane = Plane(this)

  def contains(p: V3F): Boolean = {
    Triangle.sameSide(p, p1, p2, p3) && Triangle.sameSide(p, p2, p1, p3) && Triangle.sameSide(p, p3, p1, p2)
  }

  def map(func: V3F => V3F): Triangle =
    Triangle(func(p1), func(p2), func(p3))
}

object Triangle {
  def sameSide(p1: V3F, p2: V3F, a: V3F, b: V3F): Boolean = {
    val cp1 = (b - a) cross (p1 - a)
    val cp2 = (b - a) cross (p2 - a)
    (cp1 dot cp2) >= 0
  }
}