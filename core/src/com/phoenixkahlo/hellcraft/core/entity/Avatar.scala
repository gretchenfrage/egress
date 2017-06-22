package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.{ChunkEvent, World}
import com.phoenixkahlo.hellcraft.math.{Origin, V3F}

case class Avatar(
              override val direction: V3F = Origin,
              override val jumping: Boolean = false,
              override val maxVel: Float = 4.5f,
              override val jumpHeight: Float = 1.5f,
              override val pos: V3F = V3F(50, 250, 50),
              override val id: UUID = UUID.randomUUID(),
              override val rad: Float = 0.4f,
              override val height: Float = 1.99f,
              override val vel: V3F = Origin,
              override val grounded: Boolean = true
            ) extends Walker[Avatar](direction, jumping, maxVel, jumpHeight, pos, id, rad, height, vel, grounded) {

  override def updatePos(newPos: V3F): Avatar = copy(pos = newPos)

  override def updateVel(newVel: V3F): Avatar = copy(vel = newVel)

  override def updateGrounded(newGrounded: Boolean): Avatar = copy(grounded = newGrounded)

  def updateDirection(newDirection: V3F): Avatar = copy(direction = newDirection)

  def updateJumping(newJumping: Boolean): Avatar = copy(jumping = newJumping)

  override def update(world: World): Seq[ChunkEvent] = {
    super.update(world)
  }

}
