package com.phoenixkahlo.hellcraft.util.collections

import com.phoenixkahlo.hellcraft.math.{Ones, V3I}

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom

sealed trait V3ISet extends Set[V3I] {
  def bloat: V3ISet

  def shrink: V3ISet

  protected def toNormalSet: NormalV3ISet = NormalV3ISet(Set(iterator.toSeq: _*))

  override def +(elem: V3I): V3ISet =
    toNormalSet + elem

  override def -(elem: V3I): V3ISet =
    toNormalSet - elem

  override def ++(other: GenTraversableOnce[V3I]): V3ISet = ???

  override def --(other: GenTraversableOnce[V3I]): V3ISet = ???
}

object V3ISet {
  val empty = new V3ISet {
    override def shrink: V3ISet = this

    override def bloat: V3ISet = this

    override def contains(elem: V3I): Boolean = false

    override def iterator: Iterator[V3I] = Iterator.empty

    override def ++(other: GenTraversableOnce[V3I]): V3ISet =
      if (other.isInstanceOf[V3ISet]) other.asInstanceOf[V3ISet]
      else NormalV3ISet(other.foldLeft(Set.empty[V3I])(_ + _))

    override def --(other: GenTraversableOnce[V3I]): V3ISet = this
  }
}

case class NormalV3ISet(set: Set[V3I]) extends V3ISet {
  override def bloat =
    NormalV3ISet(set.flatMap(_.neighbors))

  override def shrink =
    NormalV3ISet(set.filter(_.neighbors.forall(set.contains)))

  override def contains(elem: V3I) =
    set contains elem

  override def iterator =
    set iterator

  override def +(elem: V3I) =
    NormalV3ISet(set + elem)

  override def -(elem: V3I) =
    NormalV3ISet(set - elem)

  override def ++(elems: GenTraversableOnce[V3I]) =
    NormalV3ISet(set ++ elems)

  override def --(xs: GenTraversableOnce[V3I]) =
    NormalV3ISet(set -- xs)
}
/*
private object V3ISetTest extends App {
  val s1min = V3I(4, 3, 6)
  val s1max = V3I(14, 75, 50)
  //val s1min = V3I(1, 1, 1)
  //val s1max = V3I(10, 10, 10)
  val s1a = V3IRange(s1min, s1max)
  val s1b = (s1min to s1max) toSet

  val s2min = V3I(3, 5, 7)
  val s2max = V3I(86, 58, 81)
  //val s2min = V3I(11, 11, 11)
  //val s2max = V3I(20, 20, 20)
  val s2a = V3IRange(s2min, s2max)
  val s2b = (s2min to s2max) toSet

  // bloat test
  {
    val bloat1A = s1a.bloat
    val bloat1B = s1b.flatMap(_.neighbors)
    println("bloat 1: " + (bloat1A == bloat1B))

    val bloat2A = s2a.bloat
    val bloat2B = s2b.flatMap(_.neighbors)
    println("bloat 2: " + (bloat2A == bloat2B))
  }

  // shrink test
  {
    val shrink1A = s1a.shrink
    val shrink1B = s1b.filter(_.neighbors.forall(s1b.contains))
    println("shrink 1: " + (shrink1A == shrink1B))

    val shrink2A = s2a.shrink
    val shrink2B = s2b.filter(_.neighbors.forall(s2b.contains))
    println("shrink 2: " + (shrink2A == shrink2B))
  }

  // add test
  {
    val addA = s1a ++ s2a
    val addB = s1b ++ s2b
    println("add: " + (addA equals addB))
  }

  // subtract test
  {
    val subA = s1a -- s2a
    val subB = s1b -- s2b
    println("sub: " + (subA == subB))
  }

}
*/
case class V3IRange(low: V3I, high: V3I) extends V3ISet {
  override def bloat: V3IRange =
    V3IRange(low - Ones, high + Ones)

  override def shrink: V3IRange =
    V3IRange(low + Ones, high - Ones)

  override def contains(elem: V3I): Boolean =
    elem >= low && elem <= high

  override def iterator: Iterator[V3I] =
    low toAsSeq high iterator

  override def ++(other: GenTraversableOnce[V3I]) =
    if (other.isInstanceOf[V3IRange]) V3IRangeSum(this, other.asInstanceOf[V3IRange])
    else toNormalSet ++ other

  override def --(other: GenTraversableOnce[V3I]) =
    if (other.isInstanceOf[V3IRange]) V3IRangeDiff(this, other.asInstanceOf[V3IRange])
    else toNormalSet -- other

}

case class V3IRangeSum(r1: V3IRange, r2: V3IRange) extends V3ISet {
  override def bloat: V3ISet =
    V3IRangeSum(r1.bloat, r2.bloat)

  override def shrink: V3ISet =
    V3IRangeSum(r1.shrink, r2.shrink)

  override def contains(elem: V3I): Boolean =
    (r1 contains elem) || (r2 contains elem)

  override def iterator: Iterator[V3I] =
    (V3I.componentMin(r1.low, r2.low) toAsSeq V3I.componentMax(r1.high, r2.high)).filter(contains).iterator
}

case class V3IRangeDiff(r1: V3IRange, r2: V3IRange) extends V3ISet {
  override def bloat: V3ISet =
    V3IRangeSum(r1.bloat, r1.shrink)

  override def shrink: V3ISet =
    V3IRangeSum(r1.shrink, r2.bloat)

  override def contains(elem: V3I): Boolean =
    (r1 contains elem) && !(r2 contains elem)

  override def iterator: Iterator[V3I] =
    r1.iterator filterNot (r2 contains)
}
