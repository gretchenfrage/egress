package com.phoenixkahlo.hellcraft

import java.util.UUID

import com.phoenixkahlo.hellcraft.util.{Origin, V2F, V3F}

import scala.collection.immutable.HashMap

abstract class Walker[C <: Walker[C]](
                                       val direction: V3F,
                                       val jumping: Boolean,
                                       val maxVel: Float,
                                       val airAccel: Float,
                                       val jumpHeight: Float,
                                       override val pos: V3F,
                                       override val id: UUID,
                                       override val rad: Float,
                                       override val height: Float,
                                       override val vel: V3F = Origin,
                                       override val grounded: Boolean = true
            ) extends Cylindroid[C](pos, id, rad, height, vel, grounded) {

  override def naturalVelocity: V2F =
    if (direction.magnitude == 0) V2F(0, 0)
    else direction.flatten.normalize * maxVel

  override protected def transform(world: World): C = {
    val f = super.transform(world)
    if (jumping && f.grounded) f.copy("vel" -> f.vel.copy(y = Math.sqrt(2 * g * jumpHeight).toFloat))
    else f
  }

}
