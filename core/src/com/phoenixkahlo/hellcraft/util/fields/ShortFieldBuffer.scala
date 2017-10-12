package com.phoenixkahlo.hellcraft.util.fields

import com.phoenixkahlo.hellcraft.math.V3I

class ShortFieldBuffer(val size: V3I) {

  var contents = new Array[Short](size.fold(_ * _))

  def apply(v: V3I): Short =
    contents(size.compress(v))

  def update(v: V3I, n: Short) =
    contents(size.compress(v)) = n

  def immutabilize: ShortField = {
    val field = new ShortField(Left(contents), size)
    contents = null
    field
  }

}
