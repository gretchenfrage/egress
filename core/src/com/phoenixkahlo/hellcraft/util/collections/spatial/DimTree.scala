package com.phoenixkahlo.hellcraft.util.collections.spatial

import com.phoenixkahlo.hellcraft.math._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

trait TreeDim[VI, VF] {
  def repeated(n: Float): VF
  def sub(a: VF, b: VF): VF
  def add(a: VF, b: VF): VF
  def mul(a: VF, s: Float): VF
  def div(a: VF, s: Float): VF
  def dist(a: VF, b: VF): Float
  def >=(a: VF, b: VF): Boolean
  def <=(a: VF, b: VF): Boolean
  def >(a: VF, b: VF): Boolean
  def <(a: VF, b: VF): Boolean
  def upcast(v: VI): VF
  def signs: Seq[VI]
  def signOf(v: VF): VI
  def indexof(v: VI): Int
}

case class Domain[VI, VF](center: VF, range: Float, sign: Option[VI], dim: TreeDim[VI, VF]) {
  val min: VF = dim.sub(center, dim.repeated(range))
  val max: VF = dim.add(center, dim.repeated(range))
  val diagonal: Float = dim.dist(min, max)

  def contains(v: VF): Boolean = {
    dim.>=(v, min) && dim.<(v, max)
  }

  def subsign(v: VF): VI =
    dim.signOf(dim.sub(v, center))

  def subdomain(subsign: VI): Domain[VI, VF] =
    Domain[VI, VF](dim.add(center, dim.mul(dim.upcast(subsign), range / 2)), range / 2, Some(subsign), dim)

  def children: Seq[Domain[VI, VF]] =
    dim.signs.map(subdomain)

  private def maxdist(v: VF): Float =
    dim.signs.map(sign => dim.add(center, dim.mul(dim.upcast(sign), range))).map(dim.dist(_, v)).max

  def couldContain(point: VF, within: Float): Boolean = {
    val md = maxdist(point)
    if (md == Float.PositiveInfinity) true
    else if (dim.>=(point, dim.sub(center, dim.repeated(range))) && dim.<=(point, dim.add(center, dim.repeated(range)))) true
    else md - diagonal <= within
  }

}

sealed trait DimTree[+E, VI, VF] extends Map[VF, E] {
  override def +[V1 >: E](kv: (VF, V1)): DimTree[V1, VI, VF]

  override def -(key: VF): DimTree[E, VI, VF]

  def closest(point: VF, within: Float): Option[(VF, E)]

  def within(point: VF, within: Float): Seq[(VF, E)]

  def domain: Domain[VI, VF]

  def height: Int
}

private case class Empty[VI, VF](domain: Domain[VI, VF])(implicit dim: TreeDim[VI, VF]) extends DimTree[Nothing, VI, VF] {
  override def +[V1 >: Nothing](kv: (VF, V1)): DimTree[V1, VI, VF] = {
    if (domain contains kv._1) Leaf[V1, VI, VF](kv, domain)
    else throw new IllegalArgumentException(kv + " out of range " + domain)
  }

  override def -(key: VF): DimTree[Nothing, VI, VF] = this

  override def closest(point: VF, within: Float): Option[(VF, Nothing)] = None

  override def within(point: VF, within: Float): Seq[(VF, Nothing)] = Seq.empty

  override def get(key: VF): Option[Nothing] = None

  override def iterator: Iterator[(VF, Nothing)] = Iterator.empty

  override def size: Int = 0

  override def height = 0

  override def isEmpty = true

  override def nonEmpty = false
}

private case class Leaf[+E, VI, VF](elem: (VF, E), domain: Domain[VI, VF])(implicit dim: TreeDim[VI, VF]) extends DimTree[E, VI, VF] {
  override def +[V1 >: E](kv: (VF, V1)): DimTree[V1, VI, VF] = {
    if (!domain.contains(kv._1)) throw new IllegalArgumentException(kv + " out of range " + domain)

    if (kv._1 == elem._1) Leaf[V1, VI, VF](kv, domain)
    else {
      @tailrec def fork(trace: Seq[Domain[VI, VF]]): DimTree[V1, VI, VF] = {
        val s1 = trace.head.subsign(elem._1)
        val s2 = trace.head.subsign(kv._1)
        if (s1 == s2) fork(trace.head.subdomain(s1) +: trace)
        else {
          Branch(trace.head.children.map(dom => dom.sign.get match {
            case s if s == s1 => Leaf(elem, dom)
            case s if s == s2 => Leaf(kv, dom)
            case _ => Empty(dom)
          }), trace)
        }
      }
      fork(Seq(domain))
    }
  }

  override def -(key: VF): DimTree[E, VI, VF] = {
    if (key == elem._1) Empty(domain)
    else this
  }

  override def closest(point: VF, within: Float): Option[(VF, E)] = {
    if (dim.dist(elem._1, point) <= within) Some(elem)
    else None
  }

  override def within(point: VF, within: Float): Seq[(VF, E)] = {
    if (dim.dist(elem._1, point) <= within) Seq(elem)
    else Seq.empty
  }

  override def get(key: VF): Option[E] = {
    if (key == elem._1) Some(elem._2)
    else None
  }

  override def iterator: Iterator[(VF, E)] = Iterator(elem)

  override def size: Int = 1

  override def height = 1

  override def isEmpty = false

  override def nonEmpty = true
}

private case class Branch[+E, VI, VF](children: Seq[DimTree[E, VI, VF]], domains: Seq[Domain[VI, VF]])(implicit dim: TreeDim[VI, VF]) extends DimTree[E, VI, VF] {
  override def +[V1 >: E](kv: (VF, V1)): DimTree[V1, VI, VF] = {
    if (!domains.last.contains(kv._1)) throw new IllegalArgumentException(kv + " out of range " + domains)

    val (k, v) = kv
    if (domains.head.contains(k)) {
      val i = dim.indexof(domains.head.subsign(k))
      Branch(children.updated(i, children(i) + kv), domains)
    } else {
      @tailrec def fork(trace: Seq[Domain[VI, VF]], upcoming: Seq[Domain[VI, VF]]): DimTree[V1, VI, VF] = {
        val curr = upcoming.head
        if (curr.contains(k)) {
          val kSign = curr.subsign(k)
          Branch(curr.children.map(dom => dom.sign.get match {
            case s if s == kSign => Leaf(kv, dom)
            case s if s == trace.last.sign.get => Branch(children, trace)
            case _ => Empty(dom)
          }), upcoming)
        } else {
          fork(trace :+ curr, upcoming.tail)
        }
      }
      fork(Seq(domains.head), domains.drop(1))
    }
  }

  override def -(key: VF): DimTree[E, VI, VF] = {
    if (domains.head.contains(key)) {
      val sign = domains.head.subsign(key)
      val index = dim.indexof(sign)
      val newChildren = children.updated(index, children(index) - key)
      val nonEmptyChildren = newChildren.filter(_.nonEmpty)
      if (nonEmptyChildren.size == 1) {
        val nonEmptyChild = nonEmptyChildren.head
        nonEmptyChild match {
          case Leaf(kv, dom) => Leaf(kv, domains.last)
          case Branch(cldr, dom) => Branch(cldr, dom ++ domains)
          case Empty(_) => ???
        }
      } else Branch(newChildren, domains)
    } else this
  }

  override def closest(point: VF, within: Float): Option[(VF, E)] = {
    if (domains.head couldContain (point, within)) {
      val searchPattern: List[DimTree[E, VI, VF]] = children
        .filter(_.domain couldContain(point, within))
        .sortBy(c => dim.dist(c.domain.center, point))
        .toList

      def search(pattern: List[DimTree[E, VI, VF]], within: Float): List[(VF, E)] = pattern match {
        case curr :: next => curr.closest(point, within) match {
          case Some(kv) => kv :: search(next, dim.dist(kv._1, point))
          case None => search(next, within)
        }
        case Nil => Nil
      }

      search(searchPattern, within).sortBy(kv => dim.dist(kv._1, point)).headOption
    } else None
  }

  override def within(point: VF, within: Float): Seq[(VF, E)] =
    children
      .filter(_.domain couldContain(point, within))
      .flatMap(_ within(point, within))

  override def get(key: VF): Option[E] = {
    if (domains.head.contains(key)) {
      val sign = domains.head.subsign(key)
      val index = dim.indexof(sign)
      children(index).get(key)
    } else None
  }

  override def iterator: Iterator[(VF, E)] =
    children.flatten.iterator

  override def toSeq: Seq[(VF, E)] =
    children.flatten

  override def height: Int = children.map(_.height).max + 1

  override def size: Int = children.map(_.size).sum

  override def isEmpty = false

  override def nonEmpty = true

  override def domain = domains.last
}

/**
  * Degenerate case in which this essentially becomes a binary search tree.
  * This is generally for debugging purposes.
  */
object BiTree extends TreeDim[Int, Float] {
  override def repeated(n: Float) = n
  override def sub(a: Float, b: Float) = a - b
  override def add(a: Float, b: Float) = a + b
  override def mul(a: Float, s: Float) = a * s
  override def div(a: Float, s: Float) = a / s
  override def dist(a: Float, b: Float) = Math.abs(a - b)
  override def >=(a: Float, b: Float) = a >= b
  override def <=(a: Float, b: Float) = a <= b
  override def >(a: Float, b: Float) = a > b
  override def <(a: Float, b: Float) = a < b
  override def upcast(v: Int) = v
  override def indexof(v: Int) = (v & 0x2) >> 1
  override def signs = Seq(-1, 1).sortBy(indexof)
  override def signOf(v: Float) = if (v >= 0) 1 else -1

  type BiTree[+E] = DimTree[E, Int, Float]
  def empty[E](center: Float, range: Float): BiTree[E] =
    Empty(Domain(center, range, None, this))(this)
  val bigEmpty: BiTree[Nothing] = Empty(Domain(0f, Float.MaxValue, None, this))(this)
}

object QuadTree extends TreeDim[V2I, V2F] {
  override def repeated(n: Float): V2F = V2F(n, n)
  override def sub(a: V2F, b: V2F): V2F = a - b
  override def add(a: V2F, b: V2F): V2F = a + b
  override def mul(a: V2F, s: Float): V2F = a * s
  override def div(a: V2F, s: Float): V2F = a / s
  override def dist(a: V2F, b: V2F): Float = a dist b
  override def >=(a: V2F, b: V2F): Boolean = a >= b
  override def <=(a: V2F, b: V2F): Boolean = a <= b
  override def >(a: V2F, b: V2F) = a > b
  override def <(a: V2F, b: V2F) = a < b
  override def upcast(v: V2I): V2F = v

  override def indexof(v: V2I): Int = {
    ((v.xi & 0x2) >> 1) | (v.yi & 0x2)
  }

  override val signs: Seq[V2I] = {
    val signs = new Array[V2I](4)
    for {
      x <- Seq(-1, 1)
      y <- Seq(-1, 1)
    } yield {
      val sign = V2I(x, y)
      signs(indexof(sign)) = sign
    }
    signs
  }

  override def signOf(v: V2F): V2I =
    v.map(n => if (n >= 0) 1 else -1).toInts

  type QuadTree[+E] = DimTree[E, V2I, V2F]
  def empty[E](center: V2F, range: Float): QuadTree[E] =
    Empty(Domain(center, range, None, this))(this)
  val bigEmpty: QuadTree[Nothing] = Empty(Domain(Origin2D, Float.MaxValue, None, this))(this)
}

object Octree extends TreeDim[V3I, V3F] {
  override def repeated(n: Float): V3F = V3F(n, n, n)
  override def sub(a: V3F, b: V3F): V3F = a - b
  override def add(a: V3F, b: V3F): V3F = a + b
  override def mul(a: V3F, s: Float): V3F = a * s
  override def div(a: V3F, s: Float): V3F = a / 2
  override def dist(a: V3F, b: V3F): Float = a dist b
  override def >=(a: V3F, b: V3F): Boolean = a >= b
  override def <=(a: V3F, b: V3F): Boolean = a <= b
  override def >(a: V3F, b: V3F) = a > b
  override def <(a: V3F, b: V3F) = a < b
  override def upcast(v: V3I): V3F = v

  override def indexof(v: V3I): Int =
    ((v.xi & 0x2) >> 1) | (v.yi & 0x2) | ((v.zi & 0x2) << 1)

  override val signs: Seq[V3I] = {
    val signs = new Array[V3I](8)
    for {
      x <- Seq(-1, 1)
      y <- Seq(-1, 1)
      z <- Seq(-1, 1)
    } yield {
      val sign = V3I(x, y, z)
      signs(indexof(sign)) = sign
    }
    signs
  }

  override def signOf(v: V3F): V3I =
    v.map(n => if (n >= 0) 1 else -1).toInts

  type Octree[+E] = DimTree[E, V3I, V3F]
  def empty[E](center: V3F, range: Float): Octree[E] =
    Empty(Domain(center, range, None, this))(this)
  val bigEmpty: Octree[Nothing] = Empty(Domain(Origin, Float.MaxValue, None, this))(this)
}

object HexadecaTree extends TreeDim[V4I, V4F] {
  override def repeated(n: Float): V4F = V4F(n, n, n, n)
  override def sub(a: V4F, b: V4F): V4F = a - b
  override def add(a: V4F, b: V4F): V4F = a + b
  override def mul(a: V4F, s: Float): V4F = a * s
  override def div(a: V4F, s: Float): V4F = a / s
  override def dist(a: V4F, b: V4F): Float = a dist b
  override def >=(a: V4F, b: V4F): Boolean = a >= b
  override def <=(a: V4F, b: V4F): Boolean = a <= b
  override def >(a: V4F, b: V4F) = a > b
  override def <(a: V4F, b: V4F) = a < b
  override def upcast(v: V4I): V4F = v

  override def indexof(v: V4I): Int =
    ((v.xi & 0x2) >> 1) | (v.yi & 0x2) | ((v.zi & 0x2) << 1) | ((v.wi & 0x2) << 2)

  override val signs: Seq[V4I] = {
    val signs = new Array[V4I](16)
    for {
      x <- Seq(-1, 1)
      y <- Seq(-1, 1)
      z <- Seq(-1, 1)
      w <- Seq(-1, 1)
    } yield {
      val sign = V4I(x, y, z, w)
      signs(indexof(sign)) = sign
    }
    signs
  }

  override def signOf(v: V4F): V4I =
    v.map(n => if (n >= 0) 1 else -1).toInts

  type HexadecaTree[+E] = DimTree[E, V4I, V4F]
  def empty[E](center: V4F, range: Float): HexadecaTree[E] =
    Empty(Domain(center, range, None, this))(this)
  val bigEmpty: HexadecaTree[Nothing] = Empty(Domain(V4F(0, 0, 0, 0), Float.MaxValue, None, this))(this)
}

object Tree5D extends TreeDim[(Int, Int, Int, Int, Int), (Float, Float, Float, Float, Float)] {
  override def repeated(n: Float) = (n, n, n, n, n)
  override def sub(a: (Float, Float, Float, Float, Float), b: (Float, Float, Float, Float, Float)) =
    (a._1 - b._1, a._2 - b._2, a._3 - b._3, a._4 - b._4, a._5 - b._5)
  override def add(a: (Float, Float, Float, Float, Float), b: (Float, Float, Float, Float, Float)) =
    (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4, a._5 + b._5)
  override def mul(a: (Float, Float, Float, Float, Float), s: Float) =
    (a._1 * s, a._2 * s, a._3 * s, a._4 * s, a._5 * s)
  override def div(a: (Float, Float, Float, Float, Float), s: Float) =
    (a._1 / s, a._2 / s, a._3 / s, a._4 / s, a._5 / s)
  override def dist(a: (Float, Float, Float, Float, Float), b: (Float, Float, Float, Float, Float)) =
    Math.sqrt(sub(a, b).productIterator.map(_.asInstanceOf[Float]).map(n => n * n).sum).toFloat
  override def >=(a: (Float, Float, Float, Float, Float), b: (Float, Float, Float, Float, Float)) =
    a._1 >= b._1 && a._2 >= b._2 && a._3 >= b._3 && a._4 >= b._4 && a._5 >= b._5
  override def <=(a: (Float, Float, Float, Float, Float), b: (Float, Float, Float, Float, Float)) =
    a._1 <= b._1 && a._2 <= b._2 && a._3 <= b._3 && a._4 <= b._4 && a._5 <= b._5
  override def >(a: (Float, Float, Float, Float, Float), b: (Float, Float, Float, Float, Float)) =
    a._1 > b._1 && a._2 > b._2 && a._3 > b._3 && a._4 > b._4 && a._5 > b._5
  override def <(a: (Float, Float, Float, Float, Float), b: (Float, Float, Float, Float, Float)) =
    a._1 < b._1 && a._2 < b._2 && a._3 < b._3 && a._4 < b._4 && a._5 < b._5
  override def upcast(v: (Int, Int, Int, Int, Int)) =
    (v._1.toFloat, v._2.toFloat, v._3.toFloat, v._4.toFloat, v._5.toFloat)
  override def indexof(v: (Int, Int, Int, Int, Int)) =
    ((v._1 & 0x2) >> 1) | (v._2 & 0x2) | ((v._3 & 0x2) << 1) | ((v._4 & 0x2) << 2) | ((v._5 & 0x2) << 3)
  override val signs = {
    val signs = new Array[(Int, Int, Int, Int, Int)](32)
    for {
      x <- Seq(-1, 1)
      y <- Seq(-1, 1)
      z <- Seq(-1, 1)
      w <- Seq(-1, 1)
      k <- Seq(-1, 1)
    } yield {
      val sign = (x, y, z, w, k)
      signs(indexof(sign)) = sign
    }
    signs
  }

  override def signOf(v: (Float, Float, Float, Float, Float)) =
    (
      if (v._1 >= 0) 1 else -1,
      if (v._2 >= 0) 1 else -1,
      if (v._3 >= 0) 1 else -1,
      if (v._4 >= 0) 1 else -1,
      if (v._5 >= 0) 1 else -1
    )

  type Tree5D[+E] = DimTree[E, (Int, Int, Int, Int, Int), (Float, Float, Float, Float, Float)]
  def empty[E](center: (Float, Float, Float, Float, Float), range: Float): Tree5D[E] =
    Empty(Domain(center, range, None, this))(this)
  val bigEmpty: Tree5D[Nothing] = Empty(Domain((0f, 0f, 0f, 0f, 0f), Float.MaxValue, None, this))(this)
}

class NthDimension(dim: Int) extends TreeDim[Seq[Int], Seq[Float]] {
  override def repeated(n: Float) = Stream.iterate(n)(identity).take(dim)
  override def sub(a: Seq[Float], b: Seq[Float]) =
    a.zip(b).map({ case (aa, bb) => aa - bb })
  override def add(a: Seq[Float], b: Seq[Float]) =
    a.zip(b).map({ case (aa, bb) => aa + bb })
  override def mul(a: Seq[Float], s: Float) =
    a.map(_ * s)
  override def div(a: Seq[Float], s: Float) =
    a.map(_ / s)
  override def dist(a: Seq[Float], b: Seq[Float]) =
    Math.sqrt(sub(a, b).map(n => n * n).sum).toFloat
  override def >=(a: Seq[Float], b: Seq[Float]) =
    a.zip(b).forall({ case (aa, bb) => aa >= bb })
  override def <=(a: Seq[Float], b: Seq[Float]) =
    a.zip(b).forall({ case (aa, bb) => aa <= bb })
  override def >(a: Seq[Float], b: Seq[Float]) =
    a.zip(b).forall({ case (aa, bb) => aa > bb })
  override def <(a: Seq[Float], b: Seq[Float]) =
    a.zip(b).forall({ case (aa, bb) => aa < bb })
  override def upcast(v: Seq[Int]) =
    v.map(_.toFloat)
  override def indexof(v: Seq[Int]) =
    v.zip(Stream.iterate(-1)(_ + 1)).map({ case (c, s) => (c & 0x2) >> s }).foldLeft(0)(_ | _)
  override val signs = {
    @tailrec def permute(heads: Seq[Int], tails: Seq[List[Int]], len: Int): Seq[List[Int]] = {
      val buffer = new ArrayBuffer[List[Int]]
      for {
        head <- heads
        tail <- tails
      } yield buffer += (head :: tail)
      if (len == 1) buffer
      else permute(heads, buffer, len - 1)
    }
    val signs = new Array[Seq[Int]](Math.pow(2, dim).toInt)
    for (sign <- permute(Seq(-1, 1), Seq(Nil), dim)) {
      signs(indexof(sign)) = sign
    }
    signs
  }
  override def signOf(v: Seq[Float]) =
    v.map(n => if (n >= 0) 1 else -1)
}
/*
object BiTest extends App {
  var tree: BiTree[Unit] = BiTree.empty(0, Float.MaxValue)
  for (i <- 1 to 100) {
    tree += ((i, ()): (Float, Unit))
  }
  println("size = " + tree.size)
  println("height = " + tree.height)
  val point = 0
  val buffer = new ArrayBuffer[Float]
  while (tree nonEmpty) {
    val key = tree.closest(point, 10000).get._1
    buffer += key
    tree -= key
  }
  println(buffer)
}

object QuadTest extends App {
  var tree: QuadTree[Unit] = QuadTree.empty(Origin2D, Float.MaxValue)//QuadTree.empty(V2F(64, 64), 64)
  for (i <- 1 to 100) {
    tree += V2F(i, i) -> ()
  }
  println("size = " + tree.size)
  println("height = " + tree.height)
  val point = Origin2D
  val buffer = new ArrayBuffer[V2F]
  while (tree nonEmpty) {
    val key = tree.closest(point, 10000).get._1
    buffer += key
    tree -= key
  }
  println(buffer)
}

object OctTest extends App {
  var tree: Octree[Unit] = Octree.empty(Origin, Float.MaxValue)//Octree.empty(V3F(64, 64, 64), 64)
  for (i <- 1 to 100) {
    tree += V3F(i, i, i) -> ()
  }
  println("size = " + tree.size)
  println("height = " + tree.height)
  val point = Origin
  val buffer = new ArrayBuffer[V3F]
  while (tree nonEmpty) {
    val item = tree.closest(point, 10000).get
    buffer += item._1
    tree -= item._1
  }
  println(buffer)
}

object HexadecaTest extends App {
  var tree: HexadecaTree[Unit] = HexadecaTree.empty(V4F(0, 0, 0, 0), Float.MaxValue)//HexadecaTree.empty(V4F(64, 64, 64, 64), 64)
  for (i <- 1 to 100) {
    tree += V4F(i, i, i, i) -> ()
  }
  println("size = " + tree.size)
  println("height = " + tree.height)
  val point = V4F(0, 0, 0, 0)
  val buffer = new ArrayBuffer[V4F]
  while (tree nonEmpty) {
    val key = tree.closest(point, 10000).get._1
    buffer += key
    tree -= key
  }
  println(buffer)
}
*/