package com.phoenixkahlo.hellcraft.util

class Cache[T](factory: => T) {

  protected var value: Option[T] = None

  def apply() =
    this.synchronized {
      if (value.isEmpty) value = Some(factory)
      value.get
    }

  def invalidate =
    this.synchronized {
      value = None
    }

}
