package com.phoenixkahlo.hellcraft

import java.util.UUID

import com.phoenixkahlo.hellcraft.util.{Origin, RNG, V3F}

import scala.collection.immutable.HashMap

/**
  * Created by kahlo on 6/15/2017.
  */
class Avatar(
              override val direction: V3F = Origin,
              override val jumping: Boolean = false,
              override val maxVel: Float = 4.5f,
              override val airAccel: Float = 100,
              override val jumpHeight: Float = 1.5f,
              override val pos: V3F = V3F(50, 200, 50),
              override val id: UUID = UUID.randomUUID(),
              override val rad: Float = 0.4f,
              override val height: Float = 1.99f,
              override val vel: V3F = Origin,
              override val grounded: Boolean = true
            ) extends Walker[Avatar](
  direction,
  jumping,
  maxVel,
  airAccel,
  jumpHeight,
  pos,
  id,
  rad,
  height,
  vel,
  grounded) {

  override def copy(params: (String, Any)*): Avatar = {
    val map = params.foldLeft(new HashMap[String, Any]())({ case (m, p) => m.updated(p._1, p._2) })
    new Avatar(
      map.getOrElse("direction", this.direction).asInstanceOf[V3F],
      map.getOrElse("jumping", this.jumping).asInstanceOf[Boolean],
      map.getOrElse("maxVel", this.maxVel).asInstanceOf[Float],
      map.getOrElse("acceleration", this.airAccel).asInstanceOf[Float],
      map.getOrElse("jumpHeight", this.jumpHeight).asInstanceOf[Float],
      map.getOrElse("pos", this.pos).asInstanceOf[V3F],
      map.getOrElse("id", this.id).asInstanceOf[UUID],
      map.getOrElse("rad", this.rad).asInstanceOf[Float],
      map.getOrElse("height", this.height).asInstanceOf[Float],
      map.getOrElse("vel", this.vel).asInstanceOf[V3F],
      map.getOrElse("grounded", this.grounded).asInstanceOf[Boolean]
    )
  }

}
