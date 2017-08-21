package com.phoenixkahlo.hellcraft.util.fields

import java.util
import java.util.Objects

import com.phoenixkahlo.hellcraft.math.{Origin, V3I}

import scala.reflect.ClassTag

class OptionField[T <: AnyRef] private(private val data: Either[Array[T], Vector[T]], val size: V3I)(implicit tag: ClassTag[T]) {

  private def asVector: Vector[T] = data match {
    case Left(arr) => arr.to[Vector]
    case Right(vec) => vec
  }

  private def asArray: Array[T] = data match {
    case Left(arr) => arr
    case Right(vec) => vec.toArray
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

  override def equals(obj: scala.Any): Boolean = obj match {
    case field: OptionField[T] => Origin.until(size).forall(v => this(v) == field(v))
    case _ => false
  }

  override def toString: String =
    "OptionField(" + util.Arrays.toString(asArray.asInstanceOf[Array[Object]]) + ")"
    //"OptionField~" + hashCode()

}

object OptionField {

  def apply[T <: AnyRef](size: V3I, gen: V3I => Option[T])(implicit tag: ClassTag[T]): OptionField[T] = {
    val arr = java.lang.reflect.Array.newInstance(tag.runtimeClass, size.fold(_ * _)).asInstanceOf[Array[T]]
    for (i <- arr.indices)
      gen(size.decompress(i)).foreach(arr(i) = _)
    new OptionField(Left(arr), size)(tag)
  }

}