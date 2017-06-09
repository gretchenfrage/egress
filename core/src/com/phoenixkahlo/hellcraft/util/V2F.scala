package com.phoenixkahlo.hellcraft.util

case class V2F(x: Float, y: Float) {

  def +(v: V2F) =
    V2F(x + v.x, y + v.y)

  def -(v: V2F) =
    V2F(x - v.x, y - v.y)

  def *(s: Float) =
    V2F(x * s, y * s)

  def /(s: Float) =
    V2F(x / s, y / s)

  def %(s: Float) =
    V2F(x % s, y % s)

  def >(v: V2F): Boolean =
    x > v.x && y > v.y

  def <(v: V2F): Boolean =
    x < v.x && y < v.y

  def >=(v: V2F): Boolean =
    x >= v.x && y >= v.y

  def <=(v: V2F): Boolean =
    x <= v.x && y <= v.y

  def ><(v: V2F): Boolean =
    x > v.x && y < v.y

  def <>(v: V2F): Boolean =
    x < v.x && y > v.y

  lazy val direction: Float =
    Math.toDegrees(Math.atan2(y, x)).toFloat

  lazy val magnitude: Float =
    Math.sqrt(x * x + y * y).toFloat

  lazy val normalize: V2F =
    this / magnitude

  def horizontallyInflate(height: Float): V3F =
    V3F(x, height, y)

  def dist(v: V2F): Float =
    (this - v).magnitude

  def closest(vs: V2F*): V2F = {
    var bestVec: Option[V2F] = None
    var bestDist = Float.MaxValue
    for (v <- vs) {
      val dist = this dist v
      if (dist < bestDist) {
        bestVec = Some(v)
        bestDist = dist
      }
    }
    bestVec.get
  }

  override def toString: String =
    "<" + x + ", " + y + ">"

}
