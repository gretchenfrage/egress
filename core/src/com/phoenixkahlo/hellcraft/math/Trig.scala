package com.phoenixkahlo.hellcraft.math

object Trig {

  def sin(theta: Float): Float =
    Math.sin(Math.toRadians(theta)).toFloat

  def cos(theta: Float): Float =
    Math.cos(Math.toRadians(theta)).toFloat

  def tan(theta: Float): Float =
    Math.tan(Math.toRadians(theta)).toFloat

  def asin(x: Float): Float =
    Math.toDegrees(Math.asin(x)).toFloat

  def acos(x: Float): Float =
    Math.toDegrees(Math.acos(x)).toFloat

  def atan(x: Float): Float =
    Math.toDegrees(Math.atan(x)).toFloat

  def atan2(y: Float, x: Float): Float =
    Math.toDegrees(Math.atan2(y, x)).toFloat

  def clamp(n: Float, min: Float, max: Float): Float =
    if (n < min) min
    else if (n > max) max
    else n

}
