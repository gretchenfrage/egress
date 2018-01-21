package com.phoenixkahlo.hellcraft.core.util

import com.badlogic.gdx.utils.Disposable

class DisposableBox[T](in: T, dis: T => Unit) extends Disposable {
  @volatile private var state: Option[T] = Some(in)

  def query: Option[T] = state

  def get: T = state.get

  override def dispose() = {
    val before = state.get
    state = None
    dis(before)
  }
}

object DisposableBox {
  def apply[T](t: T, dis: T => Unit) = new DisposableBox(t, dis)
  def apply[T <: Disposable](t: T) = new DisposableBox[T](t, _.dispose())
}
