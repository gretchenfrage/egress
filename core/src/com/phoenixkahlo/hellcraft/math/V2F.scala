package com.phoenixkahlo.hellcraft.math

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.thesamet.spatial.{DimensionalOrdering, Metric}

@CarboniteWith(classOf[FieldNode])
case class V2F(x: Float, y: Float) extends Serializable {

  if (x != x || y != y)
    throw new AssertionError("vector component is NaN")

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

  def angleWith(v: V2F): Float =
    Trig.acos(this.normalize dot v.normalize)

  override def toString: String =
    "<" + x + ", " + y + ">"

}

object V2F {

  implicit val ordering: DimensionalOrdering[V2F] = new DimensionalOrdering[V2F] {
    override def compareProjection(dimension: Int)(x: V2F, y: V2F): Int =
      dimension match {
        case 0 => (y.x - x.x) / Math.abs(y.x - x.x) toInt
        case 1 => (y.y - x.y) / Math.abs(y.y - x.y) toInt
      }

    override def dimensions: Int = 2
  }

  implicit val metric: Metric[V2F, Float] = new Metric[V2F, Float] {
    override def distance(x: V2F, y: V2F): Float = x dist y

    override def planarDistance(dimension: Int)(x: V2F, y: V2F): Float = {
      val dd = dimension match {
        case 0 => x.x - y.x
        case 1 => x.y - y.y
      }
      dd * dd
    }
  }

}