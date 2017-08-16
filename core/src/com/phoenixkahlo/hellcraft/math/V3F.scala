package com.phoenixkahlo.hellcraft.math

import com.badlogic.gdx.math.Vector3
import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.thesamet.spatial.{DimensionalOrdering, KDTree, Metric}

/**
  * A vector of 3 floats
  */
@CarboniteWith(classOf[FieldNode])
class V3F(val x: Float, val y: Float, val z: Float) {

  if (x != x || y != y || z != z)
    throw new AssertionError("vector component is NaN")

  def +(o: V3F): V3F =
    V3F(x + o.x, y + o.y, z + o.z)

  def -(o: V3F): V3F =
    V3F(x - o.x, y - o.y, z - o.z)

  def neg: V3F =
    V3F(-x, -y, -z)

  def >(o: V3F): Boolean =
    x > o.x && y > o.y && z > o.z

  def <(o: V3F): Boolean =
    x < o.x && y < o.y && z < o.z

  def >=(o: V3F): Boolean =
    x >= o.x && y >= o.y && z >= o.z

  def <=(o: V3F): Boolean =
    x <= o.x && y <= o.y && z <= o.z

  def *(s: Float): V3F =
    V3F(x * s, y * s, z * s)

  def **(v: V3F): V3F =
    V3F(x * v.x, y * v.y, z * v.z)

  def /(s: Float): V3F =
    V3F(x / s, y / s, z / s)

  def floatSeq: Seq[Float] =
    List(x, y, z)

  def monoidFold(f: (Float, Float) => Float) =
    f(x, f(y, z))

  def toGdx = new Vector3(x, y, z)

  protected def toIntsStrategy =
    V3I(x.toInt, y.toInt, z.toInt)
  lazy val toInts = toIntsStrategy

  def floor: V3I =
    V3I(Math.floor(x).toInt, Math.floor(y).toInt, Math.floor(z).toInt)

  def ceil: V3I =
    V3I(Math.ceil(x).toInt, Math.ceil(y).toInt, Math.ceil(z).toInt)

  lazy val flatten =
    V2F(x, z)

  lazy val magnitude =
    Math.sqrt(x * x + y * y + z * z).toFloat

  def dist(v: V3F) =
    (this - v).magnitude

  def closest(vs: V3F*): Option[V3F] = {
    var bestVec: Option[V3F] = None
    var bestDist = Float.MaxValue
    for (v <- vs) {
      val dist = this dist v
      if (dist < bestDist) {
        bestVec = Some(v)
        bestDist = dist
      }
    }
    bestVec
  }

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case V3F(xx, yy, zz) => x == xx && y == yy && z == zz
      case _ => false
    }

  override def hashCode(): Int = (x, y, z).hashCode()

  override def toString: String =
    "<" + x + ", " + y + ", " + z + ">"

  def copy(x: Float = this.x, y: Float = this.y, z: Float = this.z) =
    V3F(x, y, z)

  def dot(v: V3F) =
    x * v.x + y * v.y + z * v.z

  def cross(v: V3F) =
    V3F(
      y * v.z - z * v.y,
      z * v.x - x * v.z,
      x * v.y - y * v.x
    )

  def normalize =
    this / magnitude

  def angleWith(v: V3F) =
    Trig.acos(this.normalize dot v.normalize)

  def map(func: Float => Float) =
    V3F(func(x), func(y), func(z))

}

class Repeated(val s: Float) extends V3F(s, s, s)

object V3F {

  def apply(x: Float, y: Float, z: Float): V3F =
    new V3F(x, y, z)

  def apply(gdxVec: Vector3): V3F =
    V3F(gdxVec.x, gdxVec.y, gdxVec.z)

  def unapply(v: V3F): Option[(Float, Float, Float)] =
    Some((v.x, v.y, v.z))

  implicit val ordering: DimensionalOrdering[V3F] = new DimensionalOrdering[V3F] {
    override def compareProjection(dimension: Int)(x: V3F, y: V3F): Int =
      dimension match {
        case 0 => (y.x - x.x) / Math.abs(y.x - x.x) toInt
        case 1 => (y.y - x.y) / Math.abs(y.y - x.y) toInt
        case 2 => (y.z - x.z) / Math.abs(y.z - x.z) toInt
      }

    override def dimensions: Int = 3
  }

  implicit val metric: Metric[V3F, Float] = new Metric[V3F, Float] {
    override def distance(x: V3F, y: V3F): Float =
      x dist y

    override def planarDistance(dimension: Int)(x: V3F, y: V3F): Float = {
      val dd = dimension match {
        case 0 => x.x - y.x
        case 1 => x.y - y.y
        case 2 => x.z - y.z
      }
      dd * dd
    }
  }

}

object Repeated {

  def apply(s: Float) =
    new Repeated(s)

  def unapply(r: Repeated): Option[Float] =
    Some(r.s)

}