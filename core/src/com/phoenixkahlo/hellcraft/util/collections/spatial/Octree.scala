package com.phoenixkahlo.hellcraft.util.collections.spatial

import java.io.PrintStream

import com.phoenixkahlo.hellcraft.math._

import scala.collection.mutable.ArrayBuffer

sealed trait Octree[+E] extends Map[V3F, E] {
  override def +[V1 >: E](kv: (V3F, V1)): Octree[V1]

  override def -(key: V3F): Octree[E]

  def closest(point: V3F, within: Float): Option[(V3F, E)]

  def within(point: V3F, within: Float): Seq[(V3F, E)]

  override def size: Int

  def octant: Octant

  def prettyPrint(indentation: Int = 0, out: PrintStream = System.out): Unit
}

object Octree {
  def empty[E](center: V3F, range: Float): Octree[E] = Empty(Octant(center, range))
}

private object Signs {
  def indexOf(sign: V3I): Int = {
    ((sign.xi & 0x2) >> 1) | (sign.yi & 0x2) | ((sign.zi & 0x2) << 1)
  }

  val signs = new Array[V3I](8)
  for (sign <- Seq(
    V3I(1, 1, 1),
    V3I(-1, 1, 1),
    V3I(1, -1, 1),
    V3I(1, 1, -1),
    V3I(1, -1, -1),
    V3I(-1, 1, -1),
    V3I(-1, -1, 1),
    V3I(-1, -1, -1)
  )) signs(indexOf(sign)) = sign

  def signOf(diff: V3F): V3I =
    diff.map(n => if (n > 0) 1 else -1).toInts
}

case class Sphere(center: V3F, radius: Float) {
  def inside(plane: Plane): Boolean =
    -plane.signedDistanceTo(center) > radius

  def outside(plane: Plane): Boolean =
    plane.signedDistanceTo(center) > radius

  def intersects(plane: Plane): Boolean =
    Math.abs(plane.signedDistanceTo(center)) <= radius
}

case class Octant(center: V3F, range: Float) {
  val min: V3F = center - Repeated(range)
  val max: V3F = center + Repeated(range)
  val diagonal: Float = Math.sqrt(range * range * 12).toFloat

  def contains(v: V3F): Boolean =
    v >= min && v <= max

  def children: Seq[Octant] =
    Signs.signs.toSeq.map(suboctant)

  def subsign(v: V3F): V3I =
    Signs.signOf(v - center)

  def suboctant(subSign: V3I): Octant =
    Octant(center + (subSign * range / 2), range / 2)

  def maxdist(v: V3F): Float =
    Signs.signs.toSeq.map(sign => center + (sign * range)).map(_ dist v).max

  def couldContain(point: V3F, within: Float): Boolean = {
    val md = maxdist(point)
    if (md == Float.PositiveInfinity) true
    else if (point >= (center - Repeated(range)) && point <= (center + Repeated(range))) true
    else md - diagonal <= within
  }
}

private case class Empty(octant: Octant) extends Octree[Nothing] {
  override def +[V1 >: Nothing](kv: (V3F, V1)): Leaf[V1] = {
    if (octant contains kv._1) Leaf(kv, octant)
    else throw new IllegalArgumentException(kv + " out of range " + octant)
  }

  override def -(key: V3F): Octree[Nothing] = this

  override def closest(point: V3F, within: Float): Option[(V3F, Nothing)] = None

  override def within(point: V3F, within: Float): Seq[Nothing] = Seq.empty

  override def get(key: V3F): Option[Nothing] = None

  override def iterator: Iterator[Nothing] = Iterator.empty

  override def size: Int = 0

  override def prettyPrint(indentation: Int, out: PrintStream): Unit = {
    out.println("()")
  }
}

private case class Leaf[+E](elem: (V3F, E), octant: Octant) extends Octree[E] {
  override def +[V1 >: E](kv: (V3F, V1)): Octree[V1] = {
    if (!octant.contains(kv._1)) throw new IllegalArgumentException(kv + " out of range " + octant)

    if (kv._1 == elem._1) Leaf(kv, octant)
    else Branch(octant.children.map(Empty(_)), octant) + elem + kv
  }

  override def -(key: V3F): Octree[E] = {
    if (key == elem._1) Empty(octant)
    else this
  }

  override def closest(point: V3F, within: Float): Option[(V3F, E)] = {
    if ((elem._1 dist point) <= within) Some(elem)
    else None
  }

  override def within(point: V3F, within: Float): Seq[(V3F, E)] =
    if((elem._1 dist point) <= within) Seq(elem)
    else Seq.empty

  override def get(key: V3F): Option[E] = {
    if (key == elem._1) Some(elem._2)
    else None
  }

  override def iterator = Iterator(elem)

  override def size: Int = 1

  override def prettyPrint(indentation: Int, out: PrintStream): Unit = {
    out.println(elem)
  }
}



private case class Branch[+E](children: Seq[Octree[E]], octant: Octant) extends Octree[E] {
  override def +[V1 >: E](kv: (V3F, V1)): Branch[V1] = {
    if (!octant.contains(kv._1)) throw new IllegalArgumentException(kv + " out of range " + octant)

    val (key, value) = kv
    val sign = octant.subsign(key)
    val index = Signs.indexOf(sign)
    Branch(children.updated(index, children(index) + kv), octant)
  }

  override def -(key: V3F): Branch[E] = {
    val sign = octant.subsign(key)
    val index = Signs.indexOf(sign)
    Branch(children.updated(index, children(index) - key), octant)
  }

  override def closest(point: V3F, within: Float): Option[(V3F, E)] = {
    val searchPattern: List[Octree[E]] = children
      .filter(_.octant couldContain (point, within))
      .sortBy(_.octant.center dist point)
      .toList
    def search(pattern: List[Octree[E]], within: Float): List[(V3F, E)] = pattern match {
      case curr :: next => curr.closest(point, within) match {
        case Some(kv) => kv :: search(next, kv._1 dist point)
        case None => search(next, within)
      }
      case Nil => Nil
    }
    search(searchPattern, within).sortBy(_._1 dist point).headOption
  }

  override def within(point: V3F, within: Float) =
    children
      .filter(_.octant couldContain (point, within))
      .flatMap(_ within (point, within))

  override def get(key: V3F): Option[E] = {
    val sign = octant.subsign(key)
    val index = Signs.indexOf(sign)
    children(index).get(key)
  }

  override def iterator: Iterator[(V3F, E)] =
    children.flatten.iterator

  override def toSeq: Seq[(V3F, E)] =
    children.flatten

  override def prettyPrint(indentation: Int, out: PrintStream): Unit = {
    def justify(v: V3I): String = {
      var s = v.toString
      while (s.size < 12)
        s += ' '
      s
    }

    println('{')
    for ((sign, child) <- Signs.signs.zip(children)) {
      for (_ <- 1 to (indentation + 2)) print(' ')
      print(justify(sign) + " -> ")
      child.prettyPrint(indentation + 2, out)
    }
    for (_ <- 1 to indentation) print(' '); println('}')
  }
}

object OctTest extends App {

  var octree: Octree[Unit] = Empty(Octant(Repeated(64), 64))
  for (i <- 1 to 100) {
    octree += Repeated(i) -> ()
  }
  //octree.prettyPrint()
  println("size = " + octree.size)
  val point = Origin
  val within = octree.within(point, 10)
  println(within.zip(within.map(_._1 dist point)))
  /*
  val pos = Origin
  val buffer = new ArrayBuffer[V3F]
  while (octree nonEmpty){
    val item = octree.closest(pos, 10000).get
    buffer += item._1
    octree -= item._1
  }
  println(buffer)
  */

}