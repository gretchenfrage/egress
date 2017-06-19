package com.phoenixkahlo.hellcraft.save

import com.phoenixkahlo.hellcraft.core.Chunk
import com.twitter.chill.{Kryo, ScalaKryoInstantiator}

/**
  * Global configuration for kryo, and a kryo singleton.
  */
object GlobalKryo {

  def config(kryo: Kryo): Kryo = {
    kryo.setRegistrationRequired(false)
    kryo.register(classOf[Chunk], new ChunkSerializer)
    kryo
  }

  val kryo = new ThreadLocal[Kryo] {
    override def initialValue(): Kryo = config(new ScalaKryoInstantiator().newKryo())
  }

  def apply() = kryo.get


  //lazy val kryo: Kryo = config(new ScalaKryoInstantiator().newKryo())

}
