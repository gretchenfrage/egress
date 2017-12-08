package com.phoenixkahlo.hellcraft.util.collections

import java.util.UUID

import com.phoenixkahlo.hellcraft.util.collections.TypeMatchingMap.Identified

import scala.collection.mutable.ArrayBuffer

class TypeMatchingMap[K[_ <: B], V[_ <: B], B](private val contents: Map[Any, Any]) {
  def +[T <: B](kv: (K[T], V[T])): TypeMatchingMap[K, V, B] =
    new TypeMatchingMap[K, V, B](contents + kv)

  def -(k: K[_]): TypeMatchingMap[K, V, B] =
    new TypeMatchingMap[K, V, B](contents - k)

  def apply[T <: B](k: K[T]): V[T] =
    contents(k).asInstanceOf[V[T]]

  def get[T <: B](k: K[T]): Option[V[T]] =
    contents.get(k).map(_.asInstanceOf[V[T]])

  type Pair[T <: B] = (K[T], V[T])
  def toSeq: Seq[Pair[_]] =
    contents.toSeq.map(_.asInstanceOf[Pair[_]])

  override def toString: String = contents.toString

  override def equals(obj: scala.Any): Boolean =
    if (obj.isInstanceOf[TypeMatchingMap[Identity, Identity, _]])
      obj.asInstanceOf[TypeMatchingMap[Identity, Identity, Any]].contents == this.contents
    else false

  override def hashCode(): Int = contents.hashCode()
}

object TypeMatchingMap {
  type Identified[K[_ <: B], B] = TypeMatchingMap[K, Identity, B]
  type Identifying[V[_ <: B], B] = TypeMatchingMap[Identity, V, B]

  private val _empty = new TypeMatchingMap[Identity, Identity, Any](Map.empty)
  def empty[K[_ <: B], V[_ <: B], B]: TypeMatchingMap[K, V, B] = _empty.asInstanceOf[TypeMatchingMap[K, V, B]]
}

object TypeMatchingMapTest extends App {
  sealed trait NumBox
  type BoxID[T <: NumBox] = UUID

  case class FloatBox(n: Float) extends NumBox
  case class DoubleBox(n: Double) extends NumBox
  case class IntBox(n: Int) extends NumBox

  var map: Identified[BoxID, NumBox] = TypeMatchingMap.empty[BoxID, Identity, NumBox]

  val id5f: BoxID[FloatBox] = UUID.randomUUID()
  map = map + (id5f -> FloatBox(5f))

  val id6d: BoxID[DoubleBox] = UUID.randomUUID()
  map = map + (id6d -> DoubleBox(6.0))

  val id7i: BoxID[IntBox] = UUID.randomUUID()
  map = map + (id7i -> IntBox(7))

  val f5: FloatBox = map(id5f)
  val d6: DoubleBox = map(id6d)
  val i7: IntBox = map(id7i)

  println(map)
  println(map.toSeq)
}