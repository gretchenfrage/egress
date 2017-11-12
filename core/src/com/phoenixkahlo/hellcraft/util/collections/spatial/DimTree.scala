package com.phoenixkahlo.hellcraft.util.collections.spatial

import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.collections.spatial.HexadecaTree.HexadecaTree
import com.phoenixkahlo.hellcraft.util.collections.spatial.Octree.Octree
import com.phoenixkahlo.hellcraft.util.collections.spatial.QuadTree.QuadTree

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
  def upcast(v: VI): VF
  def signs: Seq[VI]
  def signOf(v: VF): VI
  def indexof(v: VI): Int
}

case class Domain[VI, VF](center: VF, range: Float, dim: TreeDim[VI, VF]) {
  val min: VF = dim.sub(center, dim.repeated(range))
  val max: VF = dim.add(center, dim.repeated(range))
  val diagonal: Float = dim.dist(min, max)

  def contains(v: VF): Boolean = {
    dim.>=(v, min) && dim.<=(v, max)
  }

  def subsign(v: VF): VI =
    dim.signOf(dim.sub(v, center))

  def subdomain(subsign: VI): Domain[VI, VF] =
    Domain[VI, VF](dim.add(center, dim.mul(dim.upcast(subsign), range / 2)), range / 2, dim)

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
}

private case class Leaf[+E, VI, VF](elem: (VF, E), domain: Domain[VI, VF])(implicit dim: TreeDim[VI, VF]) extends DimTree[E, VI, VF] {
  override def +[V1 >: E](kv: (VF, V1)): DimTree[V1, VI, VF] = {
    if (!domain.contains(kv._1)) throw new IllegalArgumentException(kv + " out of range " + domain)

    if (kv._1 == elem._1) Leaf[V1, VI, VF](kv, domain)
    else Branch(domain.children.map(Empty(_)), domain) + elem + kv
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
}

private case class Branch[+E, VI, VF](children: Seq[DimTree[E, VI, VF]], domain: Domain[VI, VF])(implicit dim: TreeDim[VI, VF]) extends DimTree[E, VI, VF] {
  override def +[V1 >: E](kv: (VF, V1)): DimTree[V1, VI, VF] = {
    if (!domain.contains(kv._1)) throw new IllegalArgumentException(kv + " out of range " + domain)

    val (key, value) = kv
    val sign = domain.subsign(key)
    val index = dim.indexof(sign)
    Branch(children.updated(index, children(index) + kv), domain)
  }

  override def -(key: VF): DimTree[E, VI, VF] = {
    val sign = domain.subsign(key)
    val index = dim.indexof(sign)
    Branch(children.updated(index, children(index) - key), domain)
  }

  override def closest(point: VF, within: Float): Option[(VF, E)] = {
    val searchPattern: List[DimTree[E, VI, VF]] = children
      .filter(_.domain couldContain (point, within))
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
  }

  override def within(point: VF, within: Float): Seq[(VF, E)] =
    children
      .filter(_.domain couldContain(point, within))
      .flatMap(_ within(point, within))

  override def get(key: VF): Option[E] = {
    val sign = domain.subsign(key)
    val index = dim.indexof(sign)
    children(index).get(key)
  }

  override def iterator: Iterator[(VF, E)] =
    children.flatten.iterator

  override def toSeq: Seq[(VF, E)] =
    children.flatten
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
    Empty(Domain(center, range, this))(this)
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
  override def upcast(v: V3I): V3F = v

  override def indexof(v: V3I): Int =
    ((v.xi & 0x2) >> 1) | (v.yi & 0x2) | ((v.zi & 0x2) << 1)

  override def signs: Seq[V3I] = {
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
    Empty(Domain(center, range, this))(this)
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
  override def upcast(v: V4I): V4F = v

  override def indexof(v: V4I): Int =
    ((v.xi & 0x2) >> 1) | (v.yi & 0x2) | ((v.zi & 0x2) << 1) | ((v.wi & 0x2) << 2)

  override def signs: Seq[V4I] = {
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
    Empty(Domain(center, range, this))(this)
}

object QuadTest extends App {
  var tree: QuadTree[Unit] = QuadTree.empty(V2F(64, 64), 64)
  for (i <- 1 to 100) {
    tree += V2F(i, i) -> ()
  }
  println("size = " + tree.size)
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
  var tree: Octree[Unit] = Octree.empty(V3F(64, 64, 64), 64)
  for (i <- 1 to 100) {
    tree += V3F(i, i, i) -> ()
  }
  println("size = " + tree.size)
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
  var tree: HexadecaTree[Unit] = HexadecaTree.empty(V4F(64, 64, 64, 64), 64)
  for (i <- 1 to 100) {
    tree += V4F(i, i, i, i) -> ()
  }
  println("size = " + tree.size)
  val point = V4F(0, 0, 0, 0)
  val buffer = new ArrayBuffer[V4F]
  while (tree nonEmpty) {
    val key = tree.closest(point, 10000).get._1
    buffer += key
    tree -= key
  }
  println(buffer)
}