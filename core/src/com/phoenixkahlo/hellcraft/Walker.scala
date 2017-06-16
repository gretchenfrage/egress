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
    // TODO: refactor air friction shenanigans into superclass
    val dt = 1f / 60f

    val s = super.transform(world)

    if (jumping && s.grounded)
      s.copy("vel" -> (s.vel + V3F(0, Math.sqrt(2 * g * jumpHeight).toFloat, 0)))
    else if (!s.grounded && direction.magnitude != 0 && s.vel.magnitude < maxVel) {
      val accelerated = s.vel + (direction.flatten.normalize.inflate(0) * airAccel * dt)
      if (accelerated.magnitude > maxVel)
        s.copy("vel" -> (accelerated.normalize * maxVel))
      else
        s.copy("vel" -> accelerated)
    } else if (!s.grounded && direction.magnitude == 0 && s.vel.flatten.magnitude != 0) {
      val reduced = s.vel - (s.vel.flatten.normalize.inflate(0) * g * 5 * dt)
      if ((s.vel dot reduced) > 0) s.copy("vel" -> reduced)
      else s.copy("vel" -> Origin)
    } else
      s
  }

}
