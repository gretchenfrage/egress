package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.{EntID, Entity}

object Context {
  private var impl = new ThreadLocal[IContext]

  def randInt(): Int = impl.get().randInt()
  def randFloat(): Float = impl.get().randFloat()
  def randDouble(): Double = impl.get().randDouble()

  def randByte(): Byte = randInt() toByte
  def randShort(): Short = randInt() toShort
  def randLong(): Long = (randInt().toLong << 32) | randInt()
  def randUUID(): UUID = new UUID(randLong(), randLong())
  def entID[E <: Entity[_]](): EntID[E] = EntID(randUUID())

  def eventID(): EventID = impl.get().eventID()
  def time: Long = impl.get().time

  def init(i: IContext): Unit = {
    impl.set(i)
  }

  def end(): Unit = {
    impl.set(null)
  }
}

trait IContext {
  def randInt(): Int
  def randFloat(): Float
  def randDouble(): Double
  def eventID(): EventID
  def time: Long
}