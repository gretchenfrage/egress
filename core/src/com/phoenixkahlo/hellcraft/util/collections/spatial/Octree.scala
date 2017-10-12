package com.phoenixkahlo.hellcraft.util.collections.spatial

import java.io.PrintStream

import com.phoenixkahlo.hellcraft.math._

import scala.collection.mutable.ArrayBuffer

object Signs {
  val signs = Seq(V3I(1, 1, 1), V3I(-1, 1, 1), V3I(1, -1, 1), V3I(1, 1, -1), V3I(1, -1, -1), V3I(-1, 1, -1),
    V3I(-1, -1, 1), V3I(-1, -1, -1))
  def apply(): Seq[V3I] = signs
}

@deprecated("use SpatialHashMap instead", "N/A")
sealed trait Octree[+E] extends Map[V3F, E] {
  override def +[V1 >: E](kv: (V3F, V1)): Octree[V1]

  override def -(key: V3F): Octree[E]

  def closest(point: V3F): Option[(V3F, E)]

  def prettyPrint(indentation: Int = 0, out: PrintStream = System.out): Unit

  def depth: Int
}

/**
  * An octree with no contents
  */
@deprecated("use SpatialHashMap instead", "N/A")
case class EmptyOctree(center: V3F, range: Float) extends Octree[Nothing] {
  override def +[V1 >: Nothing](kv: (V3F, V1)): Octree[V1] = OctreeLeaf(center, range, kv)

  override def get(key: V3F): Option[Nothing] = None

  override def iterator: Iterator[(V3F, Nothing)] = Iterator.empty

  override def -(key: V3F): Octree[Nothing] = this

  override def size: Int = 0

  override def closest(point: V3F): Option[(V3F, Nothing)] = None

  override def prettyPrint(indentation: Int, out: PrintStream): Unit = {
    out.println("()")
  }

  override val depth = 0
}

/**
  * An octree with one content
  */
@deprecated("use SpatialHashMap instead", "N/A")
case class OctreeLeaf[+E](center: V3F, range: Float, elem: (V3F, E)) extends Octree[E] {
  override def +[V1 >: E](kv: (V3F, V1)): Octree[V1] = {
    if (kv._1 == elem._1) OctreeLeaf(center, range, kv)
    else {
      val children = Signs().map(sign => sign -> EmptyOctree(center + (sign * range / 2), range / 2)).toMap
      OctreeBranch(center, range, children) + elem + kv
    }
  }

  override def get(key: V3F): Option[E] =
    elem match {
      case (v, e) if v == key => Some(e)
      case _ => None
    }

  override def iterator: Iterator[(V3F, E)] = Iterator(elem)

  override def -(key: V3F): Octree[E] =
    elem match {
      case (v, _) if v == key => EmptyOctree(center, range)
      case _ => this
    }

  override def size: Int = 1

  override def closest(point: V3F): Option[(V3F, E)] = Some(elem)

  override def prettyPrint(indentation: Int, out: PrintStream): Unit = {
    out.println(elem)
  }

  override val depth = 1
}

@deprecated("use SpatialHashMap instead", "N/A")
case class OctreeBranch[+E](center: V3F, range: Float, children: Map[V3I, Octree[E]]) extends Octree[E] {
  override def +[V1 >: E](kv: (V3F, V1)): Octree[V1] = {
    val (key, value) = kv
    val sign = (key - center).map(n => if (n >= 0) 1 else -1).toInts
    OctreeBranch(center, range, children.updated(sign, children(sign) + kv))
  }

  override def get(key: V3F): Option[E] = {
    val sign = (key - center).map(n => if (n >= 0) 1 else -1).toInts
    children(sign).get(key)
  }

  override def iterator: Iterator[(V3F, E)] =
    children.values.flatten.iterator

  override def -(key: V3F): Octree[E] = {
    val sign = (key - center).map(n => if (n >= 0) 1 else -1).toInts
    OctreeBranch(center, range, children.updated(sign, children(sign) - key))
  }

  override def size: Int = children.values.map(_.size).sum

  override def closest(point: V3F): Option[(V3F, E)] = {
    Signs().flatMap(children(_).closest(point)).sortBy(_._1 dist point).headOption

    /*
    val pmc = point - center
    val sign = V3I(
      if (pmc.x > 0) 1 else -1,
      if (pmc.y > 0) 1 else -1,
      if (pmc.z > 0) 1 else -1
    )
    Stream(
      sign,
      V3I(-sign.xi, sign.yi, sign.zi), V3I(sign.xi, -sign.yi, sign.zi), V3I(sign.xi, sign.yi, -sign.zi),
      V3I(sign.xi, -sign.yi, -sign.zi), V3I(-sign.xi, sign.yi, -sign.zi), V3I(-sign.xi, -sign.yi, sign.zi),
      sign.neg
    ).flatMap(children(_).closest(point)).headOption
    */
    /*
    if (point == center) Signs().flatMap(children(_).closest(point)).sortBy(_._1 dist point).headOption
    else {
      val pmc = point - center
      val sign = V3I(
        if (pmc.x > 0) 1 else -1,
        if (pmc.y > 0) 1 else -1,
        if (pmc.z > 0) 1 else -1
      )
      Stream(
        sign,
        V3I(-sign.xi, sign.yi, sign.zi), V3I(sign.xi, -sign.yi, sign.zi), V3I(sign.xi, sign.yi, -sign.zi),
        V3I(sign.xi, -sign.yi, -sign.zi), V3I(-sign.xi, sign.yi, -sign.zi), V3I(-sign.xi, -sign.yi, sign.zi),
        sign.neg
      ).flatMap(children(_).closest(point)).headOption
    }
    */

  }

  override def prettyPrint(indentation: Int, out: PrintStream): Unit = {
    def justify(v: V3I): String = {
      var s = v.toString
      while (s.size < 12)
        s += ' '
      s
    }

    println('{')
    for ((sign, child) <- children) {
      for (_ <- 1 to (indentation + 2)) print(' ')
      print(justify(sign) + " -> ")
      child.prettyPrint(indentation + 2, out)
    }
    for (_ <- 1 to indentation) print(' '); println('}')
  }

  override lazy val depth = children.values.map(_.depth).max + 1
}

@deprecated("use SpatialHashMap instead", "N/A")
object OctreeTest extends App {

  val t1 = System.nanoTime()

  var octree: Octree[Unit] = EmptyOctree(Origin, Float.MaxValue)
  /*
  for (n <- 1 to 100) {
    octree += ((V3I(n, n, -n), n))
  }
  */
  for (_ <- 1 to 10000) {
    octree += ((V3F(
      Math.random().toFloat * 1000000 - 500000,
      Math.random().toFloat * 1000000 - 500000,
      Math.random().toFloat * 1000000 - 500000
    ), ()))
  }
  println(octree)
  println("size = " + octree.size)
  println("depth = " + octree.depth)
  val pos = Origin
  val buffer = new ArrayBuffer[V3F]
  while (octree nonEmpty) {
    val item = octree.closest(pos).get
    buffer += item._1
    octree -= item._1
  }
  println(buffer)

  val t2 = System.nanoTime()
  println("construction and deconstruction took " + (t2 - t1) / 1000000 + " ms")

  val sorted = buffer.to[Vector].sortBy(_ dist Origin)
  println("is sorted = " + (buffer.to[Vector] == sorted))

}