package com.phoenixkahlo.hellcraft.util

class Cache[T] (factory: => T) {

  private var value: Option[T] = None

  def apply() = {
    if (value.isEmpty)
      value = Some(factory)
    value.get
  }

  def invalidate =
    value = None

}
