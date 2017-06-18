package com.phoenixkahlo.hellcraft.math

trait Shape {

  def contains(p: V2F): Boolean

  def closestPerimiterPoint(p: V2F): V2F

}
