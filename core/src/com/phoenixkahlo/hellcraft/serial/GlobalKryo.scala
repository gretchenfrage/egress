package com.phoenixkahlo.hellcraft.serial

import java.io.ByteArrayOutputStream
import java.util

import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.serializers.FieldSerializer
import com.esotericsoftware.kryonet.rmi.ObjectSpace
import com.phoenixkahlo.hellcraft.core.{Block, BlockGrid, Chunk}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.multiplayertest._
import com.twitter.chill.{Kryo, Output, ScalaKryoInstantiator}

import scala.collection.mutable

/**
  * Global configuration for kryo, and a thread-local kryo.
  */
object GlobalKryo {

  def config(kryo: Kryo): Kryo = {
    kryo.setRegistrationRequired(false)

    // core registration
    kryo.register(classOf[BlockGrid], new BlockGridSerializer)
    kryo.register(classOf[Block], new KryoFailer)

    // session registration
    kryo.register(classOf[ServerSession])
    kryo.register(classOf[ClientSession])

    // kryonet registration
    ObjectSpace.registerClasses(kryo)

    // illegal serializations
    kryo.register(classOf[mutable.TreeMap[_, _]], new KryoFailer)

    kryo
  }

  def create(): Kryo = {
    val instantiator = new ScalaKryoInstantiator
    instantiator.setRegistrationRequired(false)
    val kryo = instantiator.newKryo()
    config(kryo)
  }

  val kryo = new ThreadLocal[Kryo] {
    override def initialValue(): Kryo = create()
  }

  def apply(): Kryo = kryo.get

}