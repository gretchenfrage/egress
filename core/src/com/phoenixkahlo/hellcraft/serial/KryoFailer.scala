package com.phoenixkahlo.hellcraft.serial

import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, Serializer}

class KryoFailer[T] extends Serializer[T] {

  override def read(kryo: Kryo, input: Input, t: Class[T]): T =
    throw new RuntimeException("illegal to serialize")

  override def write(kryo: Kryo, output: Output, obj: T): Unit =
    throw new RuntimeException("illegal to serialize")

}
