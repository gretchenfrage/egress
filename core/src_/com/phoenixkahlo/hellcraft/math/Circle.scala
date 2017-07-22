package com.phoenixkahlo.hellcraft.math

case class Circle(center: V2F, r: Float) {

  def contains(p: V2F): Boolean =
    center.dist(p) < r

}
