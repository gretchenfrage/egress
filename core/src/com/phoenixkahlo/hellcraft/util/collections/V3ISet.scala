package com.phoenixkahlo.hellcraft.util.collections

import com.phoenixkahlo.hellcraft.math.{Ones, V3I}

import scala.collection.{GenTraversableOnce, SortedSet}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

sealed trait V3ISet extends Set[V3I] {
  def bloat: V3ISet

  def shrink: V3ISet

  def shroat(n: Int): V3ISet

  protected def toNormalSet: V3IHashSet = V3IHashSet(toSet)

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

    override def shroat(n: Int) = this

    override def contains(elem: V3I): Boolean = false

    override def iterator: Iterator[V3I] = Iterator.empty

    override def ++(other: GenTraversableOnce[V3I]): V3ISet =
      if (other.isInstanceOf[V3ISet]) other.asInstanceOf[V3ISet]
      else V3IHashSet(other.foldLeft(Set.empty[V3I])(_ + _))

    override def --(other: GenTraversableOnce[V3I]): V3ISet = this
  }
}

object V3ISetTest extends App {
  /*
  val r1 = V3IRange(V3I(26, 31, 89), V3I(99, 47, 98))
  val r2 = V3IRange(V3I(44, 26, 27), V3I(45, 83, 99))
  val distinct = Distinct(r1, r2, v => r1(v) || r2(v))
  */
  //val sum = r1 ++ r2

  val rand = new Random(28937649832724L)
  for (i <- 1 to 1000) {
    val n = 100
    val r1 = V3IRange(
      V3I(rand.nextInt(n), rand.nextInt(n), rand.nextInt(n)),
      V3I(rand.nextInt(n), rand.nextInt(n), rand.nextInt(n))
    )
    val r2 = V3IRange(
      V3I(rand.nextInt(n), rand.nextInt(n), rand.nextInt(n)),
      V3I(rand.nextInt(n), rand.nextInt(n), rand.nextInt(n))
    )
    if (r1.isValid && r2.isValid) {
      val optimizedsum = r1 ++ r2
      val hashsum = r1.toSet ++ r2.toSet
      if (optimizedsum == hashsum)
        println("test " + i + " passed")
      else
        println("TEST " + i + " FAILED")
    }
  }

}

case class V3IHashSet(set: Set[V3I]) extends V3ISet {
  println("performance warning: V3I hash set created")

  override def bloat =
    V3IHashSet(set.flatMap(_.neighbors))

  override def shrink =
    V3IHashSet(set.filter(_.neighbors.forall(set.contains)))

  override def shroat(n: Int): V3ISet =
    if (n == 0) this
    else if (n > 0) bloat.shroat(n - 1)
    else shrink.shroat(n + 1)

  override def contains(elem: V3I) =
    set contains elem

  override def iterator =
    set iterator

  override def +(elem: V3I) =
    V3IHashSet(set + elem)

  override def -(elem: V3I) =
    V3IHashSet(set - elem)

  override def ++(elems: GenTraversableOnce[V3I]) =
    V3IHashSet(set ++ elems)

  override def --(xs: GenTraversableOnce[V3I]) =
    V3IHashSet(set -- xs)
}

case class V3IRange(low: V3I, high: V3I) extends V3ISet {
  if (!(high > low))
    ()//println("warning: invalid range")

  def isValid: Boolean = high >= low

  override def bloat: V3IRange =
    V3IRange(low - Ones, high + Ones)

  override def shrink: V3IRange =
    V3IRange(low + Ones, high - Ones)

  override def shroat(n: Int) =
    V3IRange(low - (Ones * n), high + (Ones * n))

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

object StreamTest extends App {
  // weird....
  def f(as: Seq[String], bs: Seq[String]): Unit =
    for {
      a <- as
      b <- bs
    } yield println((a, b))

  val seq = Seq(1, 2, 3).map(_.toString)
  f(seq, seq)

  println()

  val stream = Stream.iterate(1)(_ + 1).map(_.toString).take(3)
  f(stream, stream)

}

object Distinct extends ((V3IRange, V3IRange, V3I => Boolean) => Seq[V3IRange]) {

  /*
  override def apply(r1: V3IRange, r2: V3IRange, contains: V3I => Boolean) = {
    val vs: Seq[V3I] = Seq(r1.low, r1.high, r2.low, r2.high)
    def ranges(comp: V3I => Int): Seq[(Int, Int)] = {
      val nums = vs.map(comp).to[SortedSet].toSeq
      nums.zip(nums.tail.dropRight(1).map(_ - 1) :+ nums.last)
    }
    val xranges = ranges(_.xi)
    val yranges = ranges(_.yi)
    val zranges = ranges(_.zi)
    val xyzranges = new ArrayBuffer[V3IRange]
    for {
      xrange <- xranges
      yrange <- yranges
      zrange <- zranges
    } yield {
      val xyzrange = V3IRange(V3I(xrange._1, yrange._1, zrange._1), V3I(xrange._2, yrange._2, zrange._2))
      if (contains(xyzrange.low))
        xyzranges += xyzrange
    }
    xyzranges
  }
  */
  override def apply(r1: V3IRange, r2: V3IRange, contains: V3I => Boolean) = {
    val vs: Seq[V3I] = Seq(
      r1.low, r1.low - Ones, r2.low, r2.low - Ones,
      r1.high, r1.high + Ones, r2.high, r2.high + Ones
    )
    def ranges(comp: V3I => Int): Seq[(Int, Int)] =
      vs.map(comp).sorted.drop(1).dropRight(1).sliding(2, 2).map(seq => (seq(0), seq(1))).toSeq.distinct
    val xrs = ranges(_.xi).toBuffer
    val yrs = ranges(_.yi).toBuffer
    val zrs = ranges(_.zi).toBuffer
    val buffer = new ArrayBuffer[V3IRange]
    for {
      xr <- xrs
      yr <- yrs
      zr <- zrs
    } yield {
      val r = V3IRange(V3I(xr._1, yr._1, zr._1), V3I(xr._2, yr._2, zr._2))
      if (contains(r.low))
        buffer += r
    }
    buffer
  }
}

case class V3IRangeSum(r1: V3IRange, r2: V3IRange) extends V3ISet {
  override def bloat: V3ISet =
    V3IRangeSum(r1.bloat, r2.bloat)

  override def shrink: V3ISet =
    V3IRangeSum(r1.shrink, r2.shrink)

  override def shroat(n: Int) =
    V3IRangeSum(r1.shroat(n), r2.shroat(n))

  override def contains(elem: V3I): Boolean =
    (r1 contains elem) || (r2 contains elem)

  private lazy val distinct: Seq[V3IRange] = Distinct(r1, r2, this)

  override def iterator =
    distinct.iterator.flatMap(_.iterator)
}

case class V3IRangeDiff(r1: V3IRange, r2: V3IRange) extends V3ISet {
  override def bloat: V3ISet =
    V3IRangeDiff(r1.bloat, r1.shrink)

  override def shrink: V3ISet =
    V3IRangeDiff(r1.shrink, r2.bloat)

  override def shroat(n: Int) =
    V3IRangeDiff(r1.shroat(n), r2.shroat(-n))

  override def contains(elem: V3I): Boolean =
    (r1 contains elem) && !(r2 contains elem)

  private lazy val distinct: Seq[V3IRange] = Distinct(r1, r2, this)
  /*
  override def iterator: Iterator[V3I] =
    r1.iterator filterNot (r2 contains)
    */

  override def iterator =
    distinct.iterator.flatMap(_.iterator)
}
