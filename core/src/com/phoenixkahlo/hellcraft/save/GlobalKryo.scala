package com.phoenixkahlo.hellcraft.save

import com.esotericsoftware.kryonet.FrameworkMessage._
import com.esotericsoftware.kryonet.rmi.ObjectSpace
import com.esotericsoftware.kryonet.rmi.ObjectSpace.{InvokeMethod, InvokeMethodResult}
import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.multiplayertest._
import com.twitter.chill.{Kryo, ScalaKryoInstantiator}

import scala.collection.immutable

/**
  * Global configuration for kryo, and a thread-local kryo singleton
  */
object GlobalKryo {

  def config(kryo: Kryo): Kryo = {
    kryo.setRegistrationRequired(false)

    // core registration
    kryo.register(classOf[Chunk], new ChunkSerializer)
    kryo.register(classOf[V3I])

    // multiplayer test registration
    kryo.register(classOf[InitialClientData])
    kryo.register(classOf[InitialServerData])
    kryo.register(classOf[ClientSessionReady])
    kryo.register(classOf[ServerSessionReady])
    kryo.register(classOf[ServerSession])
    kryo.register(classOf[ClientSession])

    // kryonet registration
    ObjectSpace.registerClasses(kryo)
    /*
    kryo.register(classOf[InvokeMethod])
    kryo.register(classOf[InvokeMethodResult])
*/

    // std lib registration
    kryo.register(classOf[java.util.ArrayList[_]])
    kryo.register(classOf[immutable.TreeMap[_, _]])
    kryo.register(classOf[immutable.HashMap[_, _]])

    kryo.register(classOf[NoSuchElementException])
    kryo.register(classOf[Array[Byte]])
    kryo.register(Ordering.Long.getClass)
    kryo.register(classOf[StackTraceElement])
    kryo.register(classOf[Array[StackTraceElement]])

    kryo
  }

  def create(): Kryo =
    config(new ScalaKryoInstantiator().newKryo())

  val kryo = new ThreadLocal[Kryo] {
    override def initialValue(): Kryo = create()
  }

  def apply(): Kryo = kryo.get

}
