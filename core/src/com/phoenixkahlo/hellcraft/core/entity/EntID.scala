package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.math.MRNG

case class EntID[E <: Entity[_]](uuid: UUID)

object EntID {
  def random[E <: Entity[E]]() = new EntID[E](UUID.randomUUID())

  def apply[E <: Entity[E]]()(implicit rand: MRNG) = new EntID[E](rand.nextUUID)
}