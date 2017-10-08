package com.phoenixkahlo.hellcraft.util.spatial

import scala.collection.SortedSet

case class BiSet[T] private (a: T, b: T) extends Set[T] {
  override def +(elem: T): Set[T] = Set(a, b, elem)

  override def -(elem: T): Set[T] =
    if (elem == a) Set(b)
    else if (elem == b) Set(a)
    else this

  override def iterator: Iterator[T] =
    Iterator(a, b)

  override def contains(elem: T): Boolean =
    a == elem || b == elem

  def tuple: (T, T) =
    (a, b)
}

object BiSet {
  def apply[T](a: T, b: T): BiSet[T] = {
    if (a == b)
      throw new IllegalArgumentException("biset with two identical items")
    if (a.asInstanceOf[AnyRef].hashCode() < b.asInstanceOf[AnyRef].hashCode())
      BiSet(a, b)
    else
      BiSet(b, a)
  }
}

case class TriSet[T] private (a: T, b: T, c: T) extends Set[T] {
  override def contains(elem: T): Boolean =
    a == elem || b == elem || c == elem

  override def +(elem: T): Set[T] =
    Set(a, b, c, elem)

  override def -(elem: T): Set[T] =
    if (elem == a) Set(b, c)
    else if (elem == b) Set(a, c)
    else if (elem == c) Set(a, b)
    else this

  override def iterator: Iterator[T] =
    Iterator(a, b, c)

  def tuple: (T, T, T) =
    (a, b, c)
}

object TriSet {
  def apply[T](a: T, b: T, c: T): TriSet[T] = {
    if (a == b || b == c || a == c)
      throw new IllegalArgumentException("triset with multiple identical items")
    val sorted = Seq(a, b, c).sortBy(_.asInstanceOf[AnyRef].hashCode())
    TriSet(sorted(0), sorted(1), sorted(2))
  }
}

/**
  * Like a graph data structure, but instead of being made of edges connecting two nodes, it's made of triangular
  * faces connecting three nodes.
  */
case class TriGraph[T] private(map: Map[T, Set[BiSet[T]]], tris: Set[TriSet[T]]) extends Iterable[(T, T, T)] {
  override def iterator: Iterator[(T, T, T)] = tris.iterator.map(_.tuple)

  def +(a: T, b: T, c: T): TriGraph[T] =
    new TriGraph[T](
      map
        .updated(a, map.getOrElse(a, Set.empty) + BiSet(b, c))
        .updated(b, map.getOrElse(b, Set.empty) + BiSet(a, c))
        .updated(c, map.getOrElse(c, Set.empty) + BiSet(a, b)),
      tris + TriSet(a, b, c)
    )

  def ++(neus: (T, T, T)*): TriGraph[T] =
    neus.foldLeft(this)({ case (graph, (a, b, c)) => graph + (a, b, c) })

  def -(a: T, b: T, c: T): TriGraph[T] =
    new TriGraph[T](
      map
        .updated(a, map.getOrElse(a, Set.empty) - BiSet(b, c))
        .updated(b, map.getOrElse(b, Set.empty) - BiSet(a, c))
        .updated(c, map.getOrElse(c, Set.empty) - BiSet(a, b)),
      tris - TriSet(a, b, c)
    )

  def --(rmv: (T, T, T)*): TriGraph[T] =
    rmv.foldLeft(this)({ case (graph, (a, b, c)) => graph - (a, b, c) })

  def apply(vert: T): Seq[(T, T, T)] =
    map.getOrElse(vert, Set.empty).toSeq.map({ case BiSet(a, b) => (vert, a, b) })
}

object TriGraph {
  def empty[T]: TriGraph[T] = TriGraph(Map.empty, Set.empty)
}