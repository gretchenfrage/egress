package com.phoenixkahlo.hellcraft.util.collections

class Lazy[+T](factory: => T) extends Serializable {
  @transient lazy val get: T = factory
}
object Lazy {
  def apply[T](factory: => T): Lazy[T] = new Lazy(factory)
}