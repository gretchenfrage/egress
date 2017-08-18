package com.phoenixkahlo.hellcraft.core.util

import java.util.Objects

import com.phoenixkahlo.hellcraft.math.{ChunkSize, Origin, V3F, V3I}

class VectorField private(data: Either[Array[V3F], Vector[V3F]]) {

  private def this(data: Array[V3F]) = this(Left(data))

  private def this(data: Vector[V3F]) = this(Right(data))

  private def asVector: Vector[V3F] = data match {
    case Left(arr) => arr.to[Vector]
    case Right(vec) => vec
  }

  private def asArray: Array[V3F] = data match {
    case Left(arr) => arr
    case Right(vec) => vec.toArray
  }

  def updated(v: V3I, d: V3F): VectorField = {
    new VectorField(asVector.updated(ChunkSize.compress(v), d))
  }

  def apply(v: V3I): Option[V3F] =
    if (v >= Origin && v < ChunkSize) data match {
      case Left(arr) => Some(arr(ChunkSize.compress(v)))
      case Right(vec) => Some(vec(ChunkSize.compress(v)))
    } else None

  override def hashCode(): Int =
    Objects.hash(asArray: _*)

  override def equals(obj: scala.Any): Boolean = obj match {
    case field: VectorField => Origin.until(ChunkSize).forall(v => this(v) == field(v))
    case _ => false
  }

  override def toString: String =
    "VectorField~" + hashCode()

}

object VectorField {

  def apply(gen: V3I => V3F): VectorField = {
    val arr = new Array[V3F](4096)
    for (i <- arr.indices)
      arr(i) = gen(ChunkSize.decompress(i))
    new VectorField(arr)
  }

}
