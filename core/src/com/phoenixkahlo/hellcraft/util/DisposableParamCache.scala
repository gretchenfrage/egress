package com.phoenixkahlo.hellcraft.util


class DisposableParamCache[P, T](factory: P => T, disposer: T => Unit) extends ParamCache(factory) {

  override def invalidate: Unit = this.synchronized {
    value match {
      case Some(resource) => disposer(resource)
      case None =>
    }
    super.invalidate
  }

}