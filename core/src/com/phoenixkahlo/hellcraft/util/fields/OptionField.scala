package com.phoenixkahlo.hellcraft.util.fields

import java.util
import java.util.Objects

import com.phoenixkahlo.hellcraft.carbonite.{CarboniteFields, CarboniteWith}
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.math.{Origin, V3I}

import scala.reflect.ClassTag

@CarboniteFields
class OptionField[T <: AnyRef] private(private val data: Either[Array[T], Vector[T]], val sizeVec: V3I)
  extends Iterable[T] with Serializable {

  def this() = this(null: Either[Array[T], Vector[T]], null: V3I)

  override val isEmpty: Boolean = Origin.untilAsSeq(sizeVec).map(apply).forall(_.isEmpty)
  override val nonEmpty: Boolean = !isEmpty

  private def asVector: Vector[T] = data match {
    case Left(arr) => arr.to[Vector]
    case Right(vec) => vec
  }

  private def asArray: Array[T] = data match {
    case Left(arr) => arr
    case Right(vec) => vec.toArray[AnyRef].asInstanceOf[Array[T]]
  }

  def updated(v: V3I, d: T): OptionField[T] = {
    new OptionField[T](Right(asVector.updated(sizeVec.compress(v), d)), sizeVec)
  }

  def apply(v: V3I): Option[T] =
    if (v >= Origin && v < sizeVec) data match {
      case Left(arr) => Option(arr(sizeVec.compress(v)))
      case Right(vec) => Option(vec(sizeVec.compress(v)))
    } else None

  override def hashCode(): Int =
    Objects.hash(asArray: _*)

  def map[N >: Null <: AnyRef](func: T => N) = new OptionField(Right(asVector.map({
    case null => null
    case t => func(t)
  })), sizeVec)

  override def iterator: Iterator[T] =
    Origin.untilAsSeq(sizeVec).iterator.flatMap(apply)

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case value: AnyRef if this.eq(value) => true
      case field: OptionField[T] => Origin.untilAsSeq(sizeVec).forall(v => this (v) == field(v))
      case _ => false
    }

  override def toString: String =
    "OptionField(" + util.Arrays.toString(asArray.asInstanceOf[Array[Object]]) + ")"

}

object OptionField {

  def apply[T <: AnyRef](size: V3I, gen: V3I => Option[T]): OptionField[T] = {
    val arr = new Array[AnyRef](size.fold(_ * _)).asInstanceOf[Array[T]]
    for (i <- arr.indices)
      gen(size.decompress(i)).foreach(arr(i) = _)
    new OptionField(Left(arr), size)
  }

  def empty[T <: AnyRef](size: V3I): OptionField[T] = {
    val arr = new Array[AnyRef](size.fold(_ * _)).asInstanceOf[Array[T]]
    new OptionField(Left(arr), size)
  }

}