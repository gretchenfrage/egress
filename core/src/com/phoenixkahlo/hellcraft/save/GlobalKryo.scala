package com.phoenixkahlo.hellcraft.save

import com.phoenixkahlo.hellcraft.core.Chunk
import com.twitter.chill.{Kryo, ScalaKryoInstantiator}

/**
  * Global configuration for kryo, and a thread-local kryo singleton
  */
object GlobalKryo {

  def config(kryo: Kryo): Kryo = {
    kryo.setRegistrationRequired(false)
    kryo.register(classOf[Chunk], new ChunkSerializer)
    kryo
  }

  def create(): Kryo =
    config(new ScalaKryoInstantiator().newKryo())

  val kryo = new ThreadLocal[Kryo] {
    override def initialValue(): Kryo = create()
  }

  def apply() = kryo.get

}
