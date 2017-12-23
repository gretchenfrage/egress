package com.phoenixkahlo.hellcraft.util.caches

class Cache[T](factory: => T) {

  protected var value: Option[T] = None

  def apply() =
    this.synchronized {
      if (value.isEmpty) value = Some(factory)
      value.get
    }

  def query: Option[T] = value

  def invalidate =
    this.synchronized {
      value = None
    }

}