package com.phoenixkahlo.hellcraft.math.physics

import com.phoenixkahlo.hellcraft.math.V3F

case class Plane(origin: V3F, normal: V3F) {
  val eq0: Float = normal.x
  val eq1: Float = normal.y
  val eq2: Float = normal.z
  val eq3: Float = -(normal.x * origin.x + normal.y * origin.y + normal.z * origin.z)

  def signedDistanceTo(point: V3F): Double =
    (point dot normal) + eq3

  def absDistanceTo(point: V3F): Double =
    Math.abs(signedDistanceTo(point))
}

object Plane {
  def apply(tri: Triangle): Plane =
    Plane(tri.p1, ((tri.p2 - tri.p1) cross (tri.p3 - tri.p1)) normalize)
}