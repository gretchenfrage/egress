package com.phoenixkahlo.hellcraft.core.entity

import com.phoenixkahlo.hellcraft.core.{ChunkEvent, RenderWorld, World}
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.math.physics.ComplexCollider

case class Avatar(pos0: V3F, pos1: V3F, vel: V3F, stride: V3F, id: EntID[Avatar]) extends SelfUpdating[Avatar] with Moveable[Avatar] {
  override def selfUpdate(world: World) = {
    val broadphase = chunkPos.neighbors.flatMap(world.chunkAt).map(_.broadphase).reduce(_ + _)

    val before = ComplexCollider(pos, vel + (Down * 9.8f * Delta.dtf), Repeated(1), 1, Delta.dtf, 1, stride)
    val after = before.update(broadphase)

    println(stride)

    copy(pos0 = pos1, pos1 = after.pos, vel = after.vel)
  }

  override def pos = pos1

  def pos(interp: Float): V3F = pos1 + ((pos0 - pos1) * interp)

  override def updatePos(pos2: V3F) = copy(pos0 = pos1, pos1 = pos2)

  override def render(world: RenderWorld) = Seq.empty
}

object Avatar {
  def apply(pos: V3F)(implicit rand: MRNG): Avatar =
    new Avatar(pos, pos, Origin, Origin, EntID())

  def setStride(ava: Avatar, stride: V3F)(implicit rand: MRNG): ChunkEvent =
    ChunkEvent.entEvent(ava.chunkPos, ava.id)((ava, world) => ava.copy(stride = stride))
}