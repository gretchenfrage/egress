package com.phoenixkahlo.hellcraft.util.caches

import com.badlogic.gdx.utils.Disposable

class DisposableCache[T](factory: => T, disposer: T => Unit) extends Cache[T](factory) {

  override def invalidate: Unit = this.synchronized {
    value match {
      case Some(resource) => disposer(resource)
      case None =>
    }
    super.invalidate
  }

}

object DisposableCache {

  def apply[T <: Disposable](factory: => T) =
    new DisposableCache[T](factory, _.dispose())

}