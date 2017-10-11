package com.phoenixkahlo.hellcraft.util.fields

import java.util.Objects

import com.phoenixkahlo.hellcraft.carbonite.CarboniteFields
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.math.{Origin, V3I}

@CarboniteFields
class RefField[T <: AnyRef] private(private val data: Either[Array[T], Vector[T]], val sizeVec: V3I)
  extends Iterable[T] {

  override def isEmpty: Boolean = sizeVec == Origin

  private def asVector: Vector[T] = data match {
    case Left(arr) => arr.to[Vector]
    case Right(vec) => vec
  }

  private def asArray: Array[T] = data match {
    case Left(arr) => arr
    case Right(vec) => vec.toArray[AnyRef].asInstanceOf[Array[T]]
  }

  def updated(v: V3I, d: T): RefField[T] = {
    new RefField[T](Right(asVector.updated(sizeVec.compress(v), d)), sizeVec)
  }

  def get(v: V3I): Option[T] =
    if (v >= Origin && v < sizeVec) data match {
      case Left(arr) => Some(arr(sizeVec.compress(v)))
      case Right(vec) => Some(vec(sizeVec.compress(v)))
    } else None

  def apply(v: V3I): T =
    data match {
      case Left(arr) => arr(sizeVec.compress(v))
      case Right(vec) => vec(sizeVec.compress(v))
    }

  override def hashCode(): Int =
    Objects.hash(asArray: _*)

  override def iterator: Iterator[T] =
    Origin.until(sizeVec).iterator.map(apply)

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case value: AnyRef if this.eq(value) => true
      case field: RefField[T] => Origin.until(sizeVec).forall(v => this(v) == field(v))
      case _ => false
    }

  override def toString(): String =
    "RefField(" + java.util.Arrays.toString(asArray.asInstanceOf[Array[Object]]) + ")"
}

object RefField {

  def apply[T <: AnyRef](size: V3I, gen: V3I => T): RefField[T] = {
    val arr = new Array[AnyRef](size.fold(_ * _)).asInstanceOf[Array[T]]
    for (i <- arr.indices)
      arr(i) = gen(size.decompress(i))
    new RefField[T](Left(arr), size)
  }

}