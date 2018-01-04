package com.phoenixkahlo.hellcraft.core

package object entity {
  type AnyEnt = Entity[_ <: Entity[_]]
  type AnyEntID = EntID[_ <: Entity[_]]
}
