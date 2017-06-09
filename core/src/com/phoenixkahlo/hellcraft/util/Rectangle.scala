package com.phoenixkahlo.hellcraft.util

case class Rectangle(min: V2F, max: V2F) {

  def contains(p: V2F): Boolean =
    p >= min && p <= max

  lazy val center = (min + max) / 2

}
