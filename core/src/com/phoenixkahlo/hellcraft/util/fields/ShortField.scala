package com.phoenixkahlo.hellcraft.util.fields

import java.io._
import java.util.Objects
import java.util.zip.{Deflater, Inflater}

import com.phoenixkahlo.hellcraft.math.{Origin, V3I}

class ShortField private[fields](private var data: Either[Array[Short], Vector[Short]], private var _size: V3I) extends Externalizable {

  private def this(data: Array[Short], size: V3I) = this(Left(data), size)

  private def this(data: Vector[Short], size: V3I) = this(Right(data), size)

  def size: V3I = _size

  if (size.xi != size.yi || size.yi != size.zi)
    throw new IllegalArgumentException("short field must have equal dimensions")

  private def asVector: Vector[Short] = data match {
    case Left(arr) => arr.to[Vector]
    case Right(vec) => vec
  }

  private def asArray: Array[Short] = data match {
    case Left(arr) => arr
    case Right(vec) => vec.toArray
  }

  def updated(v: V3I, n: Short): ShortField =
    new ShortField(asVector.updated(size.compress(v), n), size)

  def get(v: V3I): Option[Short] =
    if (v >= Origin && v < size) data match {
      case Left(arr) => Some(arr(size.compress(v)))
      case Right(vec) => Some(vec(size.compress(v)))
    } else None

  def apply(v: V3I): Short =
    data match {
      case Left(arr) => arr(size.compress(v))
      case Right(vec) => vec(size.compress(v))
    }

  def atMod(v: V3I): Short =
    data match {
      case Left(arr) => arr(size.compress(v % size.xi))
      case Right(vec) => vec(size.compress(v % size.xi))
    }

  override def hashCode(): Int =
    Objects.hash(asArray)

  override def equals(obj: scala.Any): Boolean =
    if (obj.isInstanceOf[AnyRef] && this.eq(obj.asInstanceOf[AnyRef])) true
    else obj match {
      case field: ShortField => Origin.until(size).forall(v => this(v) == field(v))
    }

  override def toString: String =
    "ShortField(" + java.util.Arrays.toString(asArray) + ")"

  override def writeExternal(out: ObjectOutput): Unit = {
    // write size
    out.writeInt(size.xi); out.writeInt(size.yi); out.writeInt(size.zi)

    // convert to byte array
    val baos = new ByteArrayOutputStream(size.fold(_ * _) * 4)
    val dos = new DataOutputStream(baos)
    for (v <- Origin until size) {
      dos.writeShort(atMod(v))
    }
    val asBytes = baos.toByteArray

    // compress
    val compressor = new Deflater
    compressor.setInput(asBytes)
    compressor.finish()
    val buffer = new Array[Byte](asBytes.size * 2)
    val length = compressor.deflate(buffer)

    // write compressed data
    out.writeShort(length)
    out.write(buffer, 0, length)
  }

  override def readExternal(in: ObjectInput): Unit = {
    // read size
    _size = V3I(in.readInt(), in.readInt(), in.readInt())

    // read compressed data
    val length = in.readShort()
    val buffer = new Array[Byte](length)
    in.read(buffer, 0, length)

    // decompress
    val asBytes = new Array[Byte](size.fold(_ * _) * 4)
    val decompressor = new Inflater
    decompressor.setInput(buffer, 0, length)
    decompressor.inflate(asBytes)

    // convert to short array
    val shorts = new Array[Short](size.fold(_ * _))
    val dis = new DataInputStream(new ByteArrayInputStream(asBytes))
    for (v <- Origin until size) {
      shorts(size.compress(v)) = dis.readShort()
    }

    // set data
    data = Left(shorts)
  }
}

object ShortField {
  def apply(size: V3I, gen: V3I => Short): ShortField = {
    val arr = new Array[Short](size.fold(_ * _))
    for (i <- arr.indices)
      arr(i) = gen(size.decompress(i))
    new ShortField(arr, size)
  }

  def apply(size: V3I, const: Short = 0): ShortField =
    apply(size, _ => const)
}