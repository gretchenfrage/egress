package com.phoenixkahlo.hellcraft.core.entity

import com.phoenixkahlo.hellcraft.core.{PutEnt, CallService, UpdateEffect}
import com.phoenixkahlo.hellcraft.fgraphics.{PhysTID, SheetTextureID}
import com.phoenixkahlo.hellcraft.math.{MRNG, Ones, V3F}
import com.phoenixkahlo.hellcraft.service.PhysicsService
import com.phoenixkahlo.hellcraft.service.PhysicsService.{Act, Body, Sphere}


case class BulletCube(override val pos: V3F, vel: V3F, override val lastPos: V3F, override val id: EntID[BulletCube]) extends Cube[BulletCube] {
  override def tid: SheetTextureID = PhysTID

  override def update: Seq[UpdateEffect] = {
    val body = Body(Sphere(0.5f), pos, vel, 1, 1, Some(Ones))
    Seq(CallService[PhysicsService, Body](Act(body), (body: Body) => {
      val replacement = copy(lastPos = pos, pos = body.pos, vel = body.vel)
      Seq(PutEnt(replacement))
    }))
  }
}

object BulletCube {
  def apply(vel: V3F, pos: V3F)(implicit rand: MRNG): BulletCube =
    BulletCube(vel, pos, pos, EntID())
}
