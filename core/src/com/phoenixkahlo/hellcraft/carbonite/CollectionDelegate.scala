package com.phoenixkahlo.hellcraft.carbonite

import scala.collection.{SortedMap, SortedSet}

class SeqDelegate[T] extends Seq[T] {
  var source: Seq[T] = _

  override def length: Int = source.length

  override def apply(idx: Int): T = source(idx)

  override def iterator: Iterator[T] = source.iterator
}

class MapDelegate[K, V] extends Map[K, V] {
  var source: Map[K, V] = _

  override def +[V1 >: V](kv: (K, V1)): Map[K, V1] = source + kv

  override def get(key: K): Option[V] = source.get(key)

  override def iterator: Iterator[(K, V)] = source.iterator

  override def -(key: K): Map[K, V] = source - key
}

class SetDelegate[E] extends Set[E] {
  var source: Set[E] = _

  override def contains(elem: E): Boolean = source contains elem

  override def +(elem: E): Set[E] = source + elem

  override def -(elem: E): Set[E] = source - elem

  override def iterator: Iterator[E] = source.iterator
}

class SortedMapDelegate[K, V] extends SortedMap[K, V] {
  var source: SortedMap[K, V] = _

  override implicit def ordering: Ordering[K] = source.ordering

  override def rangeImpl(from: Option[K], until: Option[K]): SortedMap[K, V] = source.rangeImpl(from, until)

  override def +[B1 >: V](kv: (K, B1)): SortedMap[K, B1] = source + kv

  override def iteratorFrom(start: K): Iterator[(K, V)] = source.iteratorFrom(start)

  override def valuesIteratorFrom(start: K): Iterator[V] = source.valuesIteratorFrom(start)

  override def keysIteratorFrom(start: K): Iterator[K] = source.keysIteratorFrom(start)

  override def get(key: K): Option[V] = source.get(key)

  override def iterator: Iterator[(K, V)] = source.iterator

  override def -(key: K): SortedMap[K, V] = source - key
}

class SortedSetDelegate[E] extends SortedSet[E] {
  var source: SortedSet[E] = _

  override implicit def ordering: Ordering[E] = source.ordering

  override def rangeImpl(from: Option[E], until: Option[E]): SortedSet[E] = source.rangeImpl(from, until)

  override def keysIteratorFrom(start: E): Iterator[E] = source.keysIteratorFrom(start)

  override def contains(elem: E): Boolean = source contains elem

  override def +(elem: E): SortedSet[E] = source + elem

  override def -(elem: E): SortedSet[E] = source - elem

  override def iterator: Iterator[E] = source.iterator
}