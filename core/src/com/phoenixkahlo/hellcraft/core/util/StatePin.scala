package com.phoenixkahlo.hellcraft.core.util

import java.io.{Externalizable, ObjectInput, ObjectOutput}
import java.util.UUID
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.phoenixkahlo.hellcraft.util.threading.Fut
import com.sun.media.jfxmediaimpl.MediaDisposer.Disposable

case class StatePinKey[T, H](gen: H => T, disposer: T => Unit)

class StatePin extends Serializable {
  /*
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

*/
  @transient private var state: (Any, Disposable) = null

  def get[T, H](key: StatePinKey[T, H], hint: H): T = {
    val capture = state
    if (capture != null)
      capture._1.asInstanceOf[T]
    else this.synchronized {
      val value = key.gen(hint)
      state = (value, () => key.disposer(value))
      value
    }
  }

  def dispose(): Unit = this.synchronized {
    /*
    val k = key
    if (state != null)
      state.map(any => key.disposer.asInstanceOf[Any => Unit](any))
    state = null
    key = null
    */
    if (state != null)
      state._2.dispose()
    state = null
  }

  override def finalize(): Unit = {
    if (state != null)
      System.err.println("alarming: a state pin is being GC'd without having been disposed")
  }
}
