package com.phoenixkahlo.hellcraft.util.fields

import java.io.{Externalizable, ObjectInput, ObjectOutput}
import java.util
import java.util.Objects
import java.util.zip.{Deflater, Inflater}

import com.phoenixkahlo.hellcraft.carbonite.CarboniteFields
import com.phoenixkahlo.hellcraft.math.{Origin, V3I}

class ByteField private[fields](private var data: Either[Array[Byte], Vector[Byte]], private var _size: V3I) extends Externalizable {

  def this() = this(null: Either[Array[Byte], Vector[Byte]], null: V3I)

  private def this(data: Array[Byte], size: V3I) = this(Left(data), size)

  private def this(data: Vector[Byte], size: V3I) = this(Right(data), size)

  def size: V3I = _size

  if (size != null)
    if (size.xi != size.yi || size.yi != size.zi)
      throw new IllegalArgumentException("byte field must have equal dimensions")

  private def asVector: Vector[Byte] = data match {
    case Left(arr) => arr.to[Vector]
    case Right(vec) => vec
  }

  private def asArray: Array[Byte] = data match {
    case Left(arr) => arr
    case Right(vec) => vec.toArray
  }

  def updated(v: V3I, b: Byte): ByteField = {
    new ByteField(asVector.updated(size.compress(v), b), size)
  }

  def get(v: V3I): Option[Byte] =
    if (v >= Origin && v < size) data match {
      case Left(arr) => Some(arr(size.compress(v)))
      case Right(vec) => Some(vec(size.compress(v)))
    } else None

  def apply(v: V3I): Byte =
    data match {
      case Left(arr) => arr(size.compress(v))
      case Right(vec) => vec(size.compress(v))
    }

  def atMod(v: V3I): Byte =
    data match {
      case Left(arr) => arr(size.compress(v % size.xi))
      case Right(vec) => vec(size.compress(v % size.xi))
    }

  override def writeExternal(out: ObjectOutput): Unit = {
    out.writeInt(size.xi); out.writeInt(size.yi); out.writeInt(size.zi)

    val compressor = new Deflater
    compressor.setInput(asArray)
    compressor.finish()
    val buffer = ByteField.buffers.get
    val length = compressor.deflate(buffer)

    out.writeShort(length)
    out.write(buffer, 0, length)
  }

  override def readExternal(in: ObjectInput): Unit = {
    _size = V3I(in.readInt(), in.readInt(), in.readInt())

    val length = in.readShort()
    val buffer = ByteField.buffers.get
    in.read(buffer, 0, length)

    val arr = new Array[Byte](size.fold(_ * _))
    val decompressor = new Inflater
    decompressor.setInput(buffer, 0, length)
    decompressor.inflate(arr)
    data = Left(arr)
  }


  override def hashCode(): Int =
    Objects.hash(asArray)

  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[AnyRef] && this.eq(obj.asInstanceOf[AnyRef])) true
    else obj match {
      case field: ByteField => Origin.until(size).forall(v => this (v) == field(v))
      case _ => false
    }

  override def toString: String =
    "ByteField(" + util.Arrays.toString(asArray) + ")"

}


object ByteField {

  private val buffers = new ThreadLocal[Array[Byte]] {
    // TODO: the size shouldn't be hardcoded in
    override def initialValue(): Array[Byte] = new Array[Byte](8192)
  }

  def apply(size: V3I, gen: V3I => Byte): ByteField = {
    val arr = new Array[Byte](size.fold(_ * _))
    for (i <- arr.indices)
      arr(i) = gen(size.decompress(i))
    new ByteField(arr, size)
  }

  def apply(size: V3I, const: Byte): ByteField =
    apply(size, _ => const)

}

@CarboniteFields
case class ByteFractionField(bytes: ByteField) {

  def size: V3I = bytes.size

  def apply(v: V3I): Option[Float] =
    bytes.get(v).map(b => (b & 0xFF) / 255f)

  def atMod(v: V3I): Float =
    (bytes.atMod(v) & 0xFF) / 255f

  def updated(v: V3I, n: Float): ByteFractionField =
    ByteFractionField(bytes.updated(v, (Math.min(n, 1) * 255).toByte))

  override def toString: String =
    "FractionField(" + Origin.until(size).map(apply(_).get) + ")"

}

object ByteFractionField {

  def apply(size: V3I, gen: V3I => Float): ByteFractionField = {
    ByteFractionField(ByteField(size, v => (gen(v) * 255).toByte))
  }

}