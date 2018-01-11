package com.phoenixkahlo.hellcraft.core.util

import java.io.{Externalizable, ObjectInput, ObjectOutput}
import java.util.UUID
import java.util.concurrent.locks.ReentrantReadWriteLock

case class StatePinKey[T >: Null <: AnyRef](gen: () => T, disposer: T => Unit, id: UUID = UUID.randomUUID())

class StatePin extends Externalizable {
  private var currKey: StatePinKey[_ >: Null <: AnyRef] = null
  private var currState: AnyRef = null
  private var lock = new ReentrantReadWriteLock

  def apply[T >: Null <: AnyRef](key: StatePinKey[T]): T = {
    lock.readLock().lock()
    Option(currKey) match {
      case Some(StatePinKey(gen, dis, id)) if id == key.id =>
        try currState.asInstanceOf[T]
        finally lock.readLock.unlock()
      case _ =>
        lock.readLock().unlock()
        lock.writeLock().lock()
        Option(currKey) match {
          case Some(StatePinKey(gen, dis, id)) if id == key.id =>
            try currState.asInstanceOf[T]
            finally lock.writeLock().unlock()
          case _ =>
            Option(currKey).foreach(k => k.disposer.asInstanceOf[AnyRef => Unit](currState))
            currState = key.gen()
            try currState.asInstanceOf[T]
            finally lock.writeLock().unlock()
        }
    }
  }

  def dispose(): Unit = {
    lock.writeLock().lock()
    Option(currKey).foreach(k => k.disposer.asInstanceOf[AnyRef => Unit](currState))
    currKey = null
    lock.writeLock().unlock()
  }

  override def writeExternal(out: ObjectOutput): Unit = ()

  override def readExternal(in: ObjectInput): Unit = {
    lock = new ReentrantReadWriteLock
  }

  override def finalize(): Unit = {
    if (currKey != null)
      System.err.println("alarming: a state pin is being GC'd without having been disposed")
  }
}
