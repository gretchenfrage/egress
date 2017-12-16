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
  val rand = new Random(28937649832724L)
  for (i <- 1 to 1000) {
    val n = 10
    val r1 = V3IRange(
      V3I(rand.nextInt(n), rand.nextInt(n), /*rand.nextInt(n)*/0),
      V3I(rand.nextInt(n), rand.nextInt(n), /*rand.nextInt(n)*/0)
    )
    val r2 = V3IRange(
      V3I(rand.nextInt(n), rand.nextInt(n), /*rand.nextInt(n)*/0),
      V3I(rand.nextInt(n), rand.nextInt(n), /*rand.nextInt(n)*/0)
    )
    if (r1.isValid && r2.isValid) {
      val optimizedsum = r1 -- r2
      val hashsum = r1.toSet -- r2.toSet
      if (optimizedsum == hashsum)
        println("test " + i + " passed")
      else
        System.err.println("test " + i + " failed")
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

object Distinct extends ((V3IRange, V3IRange, V3I => Boolean) => Seq[V3IRange]) {
  private case class Range(start: Int, end: Int)
  private case class RangeState(a: Boolean, b: Boolean)
  private case class RangeBuilder(min: Int, max: Int, state: RangeState) {
    def +(n: Int) = copy(max = n)
    def build = Range(min, max)
  }
  override def apply(r1: V3IRange, r2: V3IRange, contains: V3I => Boolean) = {
    def state(n: Int)(implicit comp: V3I => Int): RangeState =
      RangeState(n >= comp(r1.low) && n <= comp(r1.high), n >= comp(r2.low) && n <= comp(r2.high))

    def ranges(curr: Option[RangeBuilder], criticals: List[Int])(implicit comp: V3I => Int): Seq[Range] = criticals match {
      case n :: tail => curr match {
        case Some(builder) if state(n) == builder.state => ranges(Some(builder + n), tail)
        case Some(builder) => builder.build +: ranges(Some(RangeBuilder(n, n, state(n))), tail)
        case None => ranges(Some(RangeBuilder(n,n, state(n))), tail)
      }
      case Nil => Nil
    }

    val criticals: List[V3I] = List(
      r1.low, r1.low - Ones, r2.low, r2.low - Ones,
      r1.high, r1.high + Ones, r2.high, r2.high + Ones
    )

    val xrs = ranges(None, criticals.map(_.xi).sorted.distinct)(_.xi)
    val yrs = ranges(None, criticals.map(_.yi).sorted.distinct)(_.yi)
    val zrs = ranges(None, criticals.map(_.zi).sorted.distinct)(_.zi)

    (for {
      xr <- xrs
      yr <- yrs
      zr <- zrs
    } yield V3IRange(V3I(xr.start, yr.start, zr.start), V3I(xr.end, yr.end, zr.end)))
      .filter(range => contains(range.low))
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

  override def iterator =
    distinct.iterator.flatMap(_.iterator)
}
