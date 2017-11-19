package com.phoenixkahlo.hellcraft.util.collections

class IdentityKey[+T <: AnyRef](obj: T) {
  override def hashCode(): Int = System.identityHashCode(obj)

  override def equals(other: scala.Any): Boolean =
    if (other.isInstanceOf[IdentityKey[_]]) other.asInstanceOf[IdentityKey[AnyRef]].obj eq this.obj
    else false

  override def toString: String = "idk(" + obj + ")"
}

object IdentityKey {
  def apply[T <: AnyRef](obj: T) = new IdentityKey(obj)
}
