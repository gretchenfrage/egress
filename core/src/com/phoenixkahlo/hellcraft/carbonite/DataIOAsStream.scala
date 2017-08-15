package com.phoenixkahlo.hellcraft.carbonite

import java.io.{DataInput, DataOutput, InputStream, OutputStream}

object DataIOAsStream {

  def apply(out: DataOutput): OutputStream =
    b => out.writeShort(b)

  def apply(in: DataInput): InputStream =
    () => in.readShort()

}
