package com.phoenixkahlo.hellcraft.serial

import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.phoenixkahlo.hellcraft.core.{Block, BlockDirectory}

class BlockSerializer extends Serializer[Block] {

  override def write(kryo: Kryo, output: Output, obj: Block): Unit = {
    output.writeByte(obj.id)
  }

  override def read(kryo: Kryo, input: Input, t: Class[Block]): Block = {
    BlockDirectory.lookup(input.readByte())
  }

}
