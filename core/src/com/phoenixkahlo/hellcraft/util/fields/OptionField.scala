package com.phoenixkahlo.hellcraft.util.fields

import java.util
import java.util.Objects

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.math.{Origin, V3I}

import scala.reflect.ClassTag

@CarboniteWith(classOf[FieldNode])
class OptionField[T <: AnyRef] private(private val data: Either[Array[T], Vector[T]], val size: V3I) {

  val isEmpty: Boolean = Origin.until(size).map(apply).forall(_.isEmpty)
  val nonEmpty: Boolean = !isEmpty

  private def asVector: Vector[T] = data match {
    case Left(arr) => arr.to[Vector]
    case Right(vec) => vec
  }

  private def asArray: Array[T] = data match {
    case Left(arr) => arr
    case Right(vec) => vec.toArray[AnyRef].asInstanceOf[Array[T]]
  }

  def updated(v: V3I, d: T): OptionField[T] = {
    new OptionField[T](Right(asVector.updated(size.compress(v), d)), size)
  }

  def apply(v: V3I): Option[T] =
    if (v >= Origin && v < size) data match {
      case Left(arr) => Option(arr(size.compress(v)))
      case Right(vec) => Option(vec(size.compress(v)))
    } else None

  override def hashCode(): Int =
    Objects.hash(asArray: _*)

  override def equals(obj: scala.Any): Boolean =
    if (obj.isInstanceOf[AnyRef] && this.eq(obj.asInstanceOf[AnyRef])) true
    else obj match {
      case field: OptionField[T] => Origin.until(size).forall(v => this (v) == field(v))
      case _ => false
    }

  override def toString: String =
    "OptionField(" + util.Arrays.toString(asArray.asInstanceOf[Array[Object]]) + ")"

}

object OptionField {

  def apply[T <: AnyRef](size: V3I, gen: V3I => Option[T]): OptionField[T] = {
    //val arr = java.lang.reflect.Array.newInstance(tag.runtimeClass, size.fold(_ * _)).asInstanceOf[Array[T]]
    val arr = new Array[AnyRef](size.fold(_ * _)).asInstanceOf[Array[T]]
    for (i <- arr.indices)
      gen(size.decompress(i)).foreach(arr(i) = _)
    new OptionField(Left(arr), size)
  }

}