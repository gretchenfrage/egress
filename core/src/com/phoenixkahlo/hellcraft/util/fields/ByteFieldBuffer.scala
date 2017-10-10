package com.phoenixkahlo.hellcraft.util.fields

import com.phoenixkahlo.hellcraft.math.V3I

class ByteFieldBuffer(val size: V3I) {

  var contents = new Array[Byte](size.fold(_ * _))

  def apply(v: V3I): Byte =
    contents(size.compress(v))

  def update(v: V3I, b: Byte): Unit =
    contents(size.compress(v)) = b

  def immutabilize: ByteField = {
    val field = new ByteField(Left(contents), size)
    contents = null
    field
  }

}

class ByteFractionFieldBuffer(val size: V3I) {

  val bytes = new ByteFieldBuffer(size)

  def apply(v: V3I): Float =
    (bytes(v) & 0xFF) / 255f

  def update(v: V3I, n: Float): Unit =
    bytes(v) = (Math.min(n, 1) * 255).toByte

  def immutabilize: ByteFractionField =
    ByteFractionField(bytes.immutabilize)

}

