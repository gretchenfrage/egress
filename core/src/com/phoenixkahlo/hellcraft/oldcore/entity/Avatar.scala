package com.phoenixkahlo.hellcraft.oldcore.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.oldcore.{ChunkEvent, World}
import com.phoenixkahlo.hellcraft.math.{Origin, V3F}

@CarboniteWith(classOf[FieldNode])
case class Avatar(
                   override val direction: V3F = Origin,
                   override val jumping: Boolean = false,
                   override val sprinting: Boolean = false,
                   override val jumpHeight: Float = 1.25f,
                   override val pos: V3F = V3F(50, 250, 50),
                   override val id: UUID = UUID.randomUUID(),
                   override val rad: Float = 0.4f,
                   override val height: Float = 1.99f,
                   override val vel: V3F = Origin,
                   override val grounded: Boolean = true,
                   override val maxVel: Float = 4.5f,
                   override val sprintVel: Float = 8f
                 ) extends Walker[Avatar](direction, jumping, sprinting, maxVel, sprintVel, jumpHeight, pos, id, rad, height, vel, grounded) {

  override def updatePos(newPos: V3F): Avatar = copy(pos = newPos)

  override def updateVel(newVel: V3F): Avatar = copy(vel = newVel)

  override def updateGrounded(newGrounded: Boolean): Avatar = copy(grounded = newGrounded)

  def updateDirection(newDirection: V3F): Avatar = copy(direction = newDirection)

  def updateJumping(newJumping: Boolean): Avatar = copy(jumping = newJumping)

  def updateSprinting(newSprinting: Boolean): Avatar = copy(sprinting = newSprinting)

  override def toString: String = "avatar " + id.toString.substring(0, 6) + " at " + pos

}
