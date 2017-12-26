package com.phoenixkahlo.hellcraft.util.collections

import java.util.UUID

import com.phoenixkahlo.hellcraft.util.collections.TypeMatchingMap.{Default, Identified}

import scala.collection.mutable.ArrayBuffer

/**
  * A map where the keys and values accept a type parameter, with a bound, and key-value pairs in the map have the same
  * type parameter.
  */
class TypeMatchingMap[K[_ <: B], V[_ <: B], B](private val contents: Map[Any, Any], default: Default[K, V, B]) {
  def +[T <: B](kv: (K[T], V[T])): TypeMatchingMap[K, V, B] =
    new TypeMatchingMap[K, V, B](contents + kv, default)

  def -(k: K[_]): TypeMatchingMap[K, V, B] =
    new TypeMatchingMap[K, V, B](contents - k, default)

  def apply[T <: B](k: K[T]): V[T] =
    get(k).get

  def get[T <: B](k: K[T]): Option[V[T]] =
    contents.get(k).map(_.asInstanceOf[V[T]]) match {
      case some@Some(v) => some
      case None => default(k)
    }

  def withDefault(d: Default[K, V, B]): TypeMatchingMap[K, V, B] =
    new TypeMatchingMap[K, V, B](contents, d)

  type ThisPair[T <: B] = TypeMatchingMap.Pair[K, V, B, T]
  def toSeq: Seq[ThisPair[_]] =
    contents.keySet.toSeq.flatMap(k => get(k.asInstanceOf[K[B]]).map(v => (k, v).asInstanceOf[ThisPair[_]]))

  override def toString: String = contents.toString

  @unchecked override def equals(obj: scala.Any): Boolean =
    if (obj.isInstanceOf[TypeMatchingMap[Identity, Identity, _]])
      obj.asInstanceOf[TypeMatchingMap[Identity, Identity, Any]].contents == this.contents
    else false

  override def hashCode(): Int = contents.hashCode()
}

object TypeMatchingMap {
  trait Default[K[_ <: B], V[_ <: B], B] {
    def apply[T <: B](k: K[T]): Option[V[T]]
  }
  object NoDefault extends Default[Identity, Identity, Any] {
    override def apply[T <: Any](k: T): Option[T] = None
  }

  type Identified[K[_ <: B], B] = TypeMatchingMap[K, Identity, B]
  type Identifying[V[_ <: B], B] = TypeMatchingMap[Identity, V, B]

  private val _empty = new TypeMatchingMap[Identity, Identity, Any](Map.empty, NoDefault)
  def empty[K[_ <: B], V[_ <: B], B]: TypeMatchingMap[K, V, B] = _empty.asInstanceOf[TypeMatchingMap[K, V, B]]

  type Pair[K[_ <: B], V[_ <: B], B, T <: B] = (K[T], V[T])
  def apply[K[_ <: B], V[_ <: B], B](seq: Pair[K, V, B, _ <: B]*): TypeMatchingMap[K, V, B] =
    seq.foldLeft(empty[K, V, B])(_ + _)
}

object TypeMatchingMapTest extends App {
  sealed trait NumBox
  type BoxID[T <: NumBox] = UUID

  case class FloatBox(n: Float) extends NumBox
  case class DoubleBox(n: Double) extends NumBox
  case class IntBox(n: Int) extends NumBox

  /*
  var map: Identified[BoxID, NumBox] = TypeMatchingMap.empty[BoxID, Identity, NumBox]


  val id5f: BoxID[FloatBox] = UUID.randomUUID()
  map = map + (id5f -> FloatBox(5f))

  val id6d: BoxID[DoubleBox] = UUID.randomUUID()
  map = map + (id6d -> DoubleBox(6.0))

  val id7i: BoxID[IntBox] = UUID.randomUUID()
  map = map + (id7i -> IntBox(7))
  */
  val id5f: BoxID[FloatBox] = UUID.randomUUID()
  val id6d: BoxID[DoubleBox] = UUID.randomUUID()
  val id7i: BoxID[IntBox] = UUID.randomUUID()

  val map = TypeMatchingMap[BoxID, Identity, NumBox](
    id5f -> FloatBox(5f),
    id6d -> DoubleBox(6d),
    id7i -> IntBox(7)
  )

  val f5: FloatBox = map(id5f)
  val d6: DoubleBox = map(id6d)
  val i7: IntBox = map(id7i)

  println(map)
  println(map.toSeq)
}