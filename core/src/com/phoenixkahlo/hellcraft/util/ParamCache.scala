package com.phoenixkahlo.hellcraft.util

class ParamCache[P, T](factory: P => T) {

  private var value: Option[T] = None

  def apply(param: P) =
    this.synchronized {
      if (value.isEmpty)
        value = Some(factory(param))
      value.get
    }

  def invalidate =
    this.synchronized {
      value = None
    }

}
