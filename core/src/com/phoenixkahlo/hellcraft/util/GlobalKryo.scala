package com.phoenixkahlo.hellcraft.util

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryonet.rmi.ObjectSpace
import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.multiplayertest._
import com.phoenixkahlo.hellcraft.save.ChunkSerializer
import com.twitter.chill.{Kryo, ScalaKryoInstantiator}

import scala.collection.{SortedMap, mutable}
import scala.collection.immutable.TreeMap

/**
  * Global configuration for kryo, and a thread-local kryo.
  */
object GlobalKryo {

  def config(kryo: Kryo): Kryo = {
    kryo.setRegistrationRequired(false)

    // core registration
    kryo.register(classOf[Chunk], new ChunkSerializer)

    // session registration
    //kryo.register(classOf[ServerSession])
    //kryo.register(classOf[ClientSession])

    // kryonet registration
    ObjectSpace.registerClasses(kryo)

    // illegal serializations
    kryo.register(classOf[mutable.TreeMap[_, _]], new KryoFailer)

    kryo
  }

  def create(): Kryo =
    config(new ScalaKryoInstantiator().newKryo())

  val kryo = new ThreadLocal[Kryo] {
    override def initialValue(): Kryo = create()
  }

  def apply(): Kryo = kryo.get

}
