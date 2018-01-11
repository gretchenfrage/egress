package com.phoenixkahlo.hellcraft.core.util

import java.io.{Externalizable, ObjectInput, ObjectOutput}
import java.util.UUID
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.phoenixkahlo.hellcraft.util.threading.Fut

case class StatePinKey[T, H](gen: H => T, disposer: T => Unit)

class StatePin extends Serializable {
  @transient private var key: StatePinKey[_, _] = null
  @transient private var state: Fut[Any] = null

  def get[T, H](key: StatePinKey[T, H], hint: H, patient: Boolean, exec: Runnable => Unit = _.run()): Fut[T] = this.synchronized {
    if (this.key == null) {
      this.key = key
      state = Fut(key.gen(hint), exec)
    }
    if (!patient && state.query.isEmpty)
      state = Fut(key.gen(hint), exec)
    state.asInstanceOf[Fut[T]]
  }

  def getPatient[T, H](key: StatePinKey[T, H], hint: H): T = get(key, hint, true, _.run()).await

  def getImpatient[T, H](key: StatePinKey[T, H], hint: H): T = get(key, hint, false, _.run()).query.get

  def prepare[T, H](key: StatePinKey[T, H], hint: H, exec: Runnable => Unit): Unit = get(key, hint, true, exec)

  def dispose(): Unit = this.synchronized {
    val k = key
    if (state != null)
      state.map(any => key.disposer.asInstanceOf[Any => Unit](any))
    state = null
    key = null
  }

  override def finalize(): Unit = {
    if (key != null)
      System.err.println("alarming: a state pin is being GC'd without having been disposed")
  }
}
