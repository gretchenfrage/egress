package com.phoenixkahlo.hellcraft.math

case class Plane(origin: V3F, nor: V3F, equation: Array[Float]) {
  def frontFacingTo(direction: V3F): Boolean =
    (nor dot direction) <= 0

  def signedDistanceTo(point: V3F): Float =
    (point dot nor) + equation(3)
}

object Plane {
  def apply(origin: V3F, nor: V3F): Plane =
    Plane(origin, nor, Array(
      nor.x,
      nor.y,
      nor.z,
      -(nor.x * origin.x + nor.y * origin.y + nor.z + origin.z)
    ))

  def apply(p1: V3F, p2: V3F, p3: V3F): Plane =
    Plane(p1, (p2 - p1) cross (p3 - p1) normalize)
}