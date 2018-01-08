package com.phoenixkahlo.hellcraft.core.util

class Derived[T >: Null <: AnyRef] extends Serializable {
  @transient private var obj: T = null

  def apply(fac: => T): T = {
    if (obj == null)
      obj = fac
    obj
  }

}