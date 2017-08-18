package com.phoenixkahlo.hellcraft.core.util

import java.io.{Externalizable, ObjectInput, ObjectOutput}
import java.util.Objects
import java.util.zip.{Deflater, Inflater}

import com.phoenixkahlo.hellcraft.math.{ChunkSize, Origin, V3I}

class ByteField private(private var data: Either[Array[Byte], Vector[Byte]]) extends Externalizable {

  private def this(data: Array[Byte]) = this(Left(data))

  private def this(data: Vector[Byte]) = this(Right(data))

  private def asVector: Vector[Byte] = data match {
    case Left(arr) => arr.to[Vector]
    case Right(vec) => vec
  }

  private def asArray: Array[Byte] = data match {
    case Left(arr) => arr
    case Right(vec) => vec.toArray
  }

  def updated(v: V3I, b: Byte): ByteField = {
    new ByteField(asVector.updated(ChunkSize.compress(v), b))
  }

  def apply(v: V3I): Option[Byte] =
    if (v >= Origin && v < ChunkSize) data match {
      case Left(arr) => Some(arr(ChunkSize.compress(v)))
      case Right(vec) => Some(vec(ChunkSize.compress(v)))
    } else None

  override def writeExternal(out: ObjectOutput): Unit = {
    val compressor = new Deflater
    compressor.setInput(asArray)
    compressor.finish()
    val buffer = ByteField.buffers.get
    val length = compressor.deflate(buffer)

    out.writeShort(length)
    out.write(buffer, 0, length)
  }

  override def readExternal(in: ObjectInput): Unit = {
    val length = in.readShort()
    val buffer = ByteField.buffers.get
    in.read(buffer, 0, length)

    val arr = new Array[Byte](4096)
    val decompressor = new Inflater
    decompressor.setInput(buffer, 0, length)
    decompressor.inflate(arr)
    data = Left(arr)
  }


  override def hashCode(): Int =
    Objects.hash(asArray: _*)

  override def equals(obj: Any): Boolean = obj match {
    case field: ByteField => Origin.until(ChunkSize).forall(v => this(v) == field(v))
    case _ => false
  }

  override def toString: String =
    "ByteField~" + hashCode()

}


object ByteField {

  private val buffers = new ThreadLocal[Array[Byte]] {
    override def initialValue(): Array[Byte] = new Array[Byte](8192)
  }

  def apply(gen: V3I => Byte): ByteField = {
    val arr = new Array[Byte](4096)
    for (i <- arr.indices)
      arr(i) = gen(ChunkSize.decompress(i))
    new ByteField(arr)
  }

  def apply(const: Byte): ByteField =
    apply(_ => const)

}