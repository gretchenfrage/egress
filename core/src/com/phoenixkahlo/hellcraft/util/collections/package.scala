package com.phoenixkahlo.hellcraft.util

package object collections {
  type Identity[T] = T

  type IdentityMap[K <: AnyRef, V] = Map[IdentityKey[K], V]
}
