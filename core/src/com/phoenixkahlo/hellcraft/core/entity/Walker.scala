package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.World
import com.phoenixkahlo.hellcraft.math.{Origin, V2F, V3F}

/*
abstract class Walker[C <: Walker[C]](
                                       val direction: V3F,
                                       val jumping: Boolean,
                                       val sprinting: Boolean,
                                       val maxVel: Float,
                                       val sprintVel: Float,
                                       val jumpHeight: Float,
                                       override val pos: V3F,
                                       override val id: UUID,
                                       override val rad: Float,
                                       override val height: Float,
                                       override val vel: V3F = Origin,
                                       override val grounded: Boolean = true
                                     ) extends Cylindroid[C](
  pos,
  id,
  rad,
  height,
  vel,
  grounded
) {

  override def naturalVelocity: V2F =
    if (direction.magnitude == 0) V2F(0, 0)
    else direction.flatten.normalize * (if (sprinting) sprintVel else maxVel)

  override protected def transform(world: World, dt: Float): C = {
    val f = super.transform(world, dt)
    if (jumping && f.grounded) f.updateVel(f.vel.copy(y = Math.sqrt(2 * g * jumpHeight).toFloat))
    else f
  }

}
*/