package com.phoenixkahlo.hellcraft.math

import com.phoenixkahlo.hellcraft.carbonite.{CarboniteFields, CarboniteWith}

@CarboniteFields
class V4I(val xi: Int, val yi: Int, val zi: Int, val wi: Int) extends V4F(xi, yi, zi, wi) {

  def +(o: V4I): V4I =
    V4I(xi + o.xi, yi + o.yi, zi + o.zi, wi + o.wi)

  def -(o: V4I): V4I =
    V4I(xi - o.xi, yi - o.yi, zi - o.zi, wi - o.wi)

  override def neg: V4I =
    V4I(-xi, -yi, -zi, -wi)

  override def toString =
    "<" + xi + ", " + yi + ", " + zi + ", " + wi + ">"

  override protected def toIntsStrategy = this
}

object V4I {
  def apply(x: Int, y: Int, z: Int, w: Int) =
    new V4I(x, y, z, w)

}