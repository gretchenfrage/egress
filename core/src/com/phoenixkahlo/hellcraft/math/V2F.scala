package com.phoenixkahlo.hellcraft.math


case class V2F(x: Float, y: Float) extends Serializable {

  if (x != x || y != y)
    throw new AssertionError("vector component is NaN")

  def +(v: V2F) =
    V2F(x + v.x, y + v.y)

  def -(v: V2F) =
    V2F(x - v.x, y - v.y)

  def *(s: Float) =
    V2F(x * s, y * s)

  def **(s: V2F) =
    V2F(x * s.x, y * s.y)

  def \\(s: V2F) =
    V2F(x / s.x, y / s.y)

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

  def dot(v: V2F): Float =
    x * v.x + y * v.y

  lazy val direction: Float =
    Trig.atan2(y, x)

  lazy val magnitude: Float =
    Math.sqrt(x * x + y * y).toFloat

  lazy val normalize: V2F =
    this / magnitude

  def inflate(height: Float): V3F =
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

  protected def toIntsStrategy: V2I = V2I(x.toInt, y.toInt)
  lazy val toInts = toIntsStrategy

  def abs: V2F =
    V2F(Math.abs(x), Math.abs(y))

  def rotate(theta: Float) =
    V2F(
      Trig.cos(direction + theta) * magnitude,
      Trig.sin(direction + theta) * magnitude
    )

  def perpendicular: (V2F, V2F) =
    (rotate(90), rotate(-90))

  def perpendicularInGeneralDirection(direction: V2F): V2F =
    perpendicular match {
      case(v1, v2) =>
        if ((v1 dot direction) >= 0) v1
        else v2
    }

  def map(func: Float => Float) =
    V2F(func(x), func(y))

  def angleWith(v: V2F): Float =
    Trig.acos(this.normalize dot v.normalize)

  override def toString: String =
    "<" + x + ", " + y + ">"

}