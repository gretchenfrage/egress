package com.phoenixkahlo.hellcraft.util

import com.twitter.chill.{Kryo, ScalaKryoInstantiator}

/**
  * Global configuration for kryo, and a kryo singleton.
  */
object GlobalKryo {

  def config(kryo: Kryo): Kryo = {
    kryo.setRegistrationRequired(false)
    kryo
  }

  lazy val kryo: Kryo = config(new ScalaKryoInstantiator().newKryo())

}
