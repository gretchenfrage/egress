package com.phoenixkahlo.hellcraft.util.caches

class ParamCache[P,T](factory: P => T) {

  protected var value: Option[T] = None

  def apply(param: P) =
    this.synchronized {
      if (value.isEmpty) value = Some(factory(param))
      value.get
    }

  def isDefined: Boolean = this.synchronized {
    value isDefined
  }

  def query: Option[T] = this.synchronized {
    value
  }

  def invalidate =
    this.synchronized {
      value = None
    }

}
