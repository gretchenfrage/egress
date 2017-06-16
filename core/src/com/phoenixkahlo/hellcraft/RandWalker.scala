package com.phoenixkahlo.hellcraft

import java.util.UUID

import com.phoenixkahlo.hellcraft.util.{Origin, RNG, V3F}

import scala.collection.immutable.HashMap

class RandWalker(
                  val rand: Stream[Float] = RNG.floats(RNG(System.nanoTime())),
                  override val direction: V3F = Origin,
                  override val jumping: Boolean = true,
                  override val maxVel: Float = 1,
                  override val airAccel: Float = 1,
                  override val jumpHeight: Float = 2,
                  override val pos: V3F = V3F(50, 200, 50),
                  override val id: UUID = UUID.randomUUID(),
                  override val rad: Float = 0.5f,
                  override val height: Float = 2,
                  override val vel: V3F = Origin,
                  override val grounded: Boolean = true
                ) extends Walker[RandWalker](direction, jumping, maxVel, airAccel, jumpHeight, pos, id, rad, height, vel, grounded) {

  override def copy(params: (String, Any)*): RandWalker = {
    val map = params.foldLeft(new HashMap[String,Any]())({ case (m, p) => m.updated(p._1, p._2) })
    new RandWalker(
      map.getOrElse("rand", this.rand).asInstanceOf[Stream[Float]],
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

  override protected def transform(world: World): RandWalker =
    super.transform(world).copy(
      "rand" -> rand.take(3),
      "direction" -> {
        val v = direction + V3F(rand.head, rand.take(1).head, rand.take(2).head)
        if (v.flatten.magnitude == 0) v
        else v.flatten.normalize.inflate(0)
      }
    )

}
