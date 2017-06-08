package com.phoenixkahlo.hellcraft.util

case class V2F(x: Float, y: Float) {

  def +(v: V2F) =
    V2F(x + v.x, y + v.y)

  def /(s: Float) =
    V2F(x / s, y / s)

  override def toString: String =
    "<" + x + ", " + y + ">"

}
