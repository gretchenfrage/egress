package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.{DoPhysics, PutEntity, Shift, TerrainSoup, UpdateEffect, World}
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.graphics.{GrassTID, PhysTID}
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.math.physics.{ComplexCollider, EmptyBroadphase, MeshRequest, Triangle}

import scala.collection.mutable.ArrayBuffer

case class PhysCube(vel: V3F, override val pos: V3F, override val id: UUID, walk: V3F, override val lastPos: V3F) extends Cube(PhysTID, pos, id) with Moveable {
  override def updatePos(newPos: V3F): Entity = copy(pos = newPos, lastPos = pos)

  def doPhysics(world: World): PhysCube = {
    val broadphase = chunkPos.neighbors.flatMap(world.chunkAt).map(_.broadphase).fold(EmptyBroadphase)(_ + _)

    var collider = ComplexCollider(pos, vel + (Down * 9.8f * Delta.dtf), Repeated(0.5f), 1000, Delta.dtf, 100, walk)
    collider = collider.update(broadphase)

    copy(vel = collider.vel, pos = collider.pos, lastPos = pos)
  }

  override def update(world: World, ids: Stream[UUID]): Seq[UpdateEffect] =
    Seq(DoPhysics(id, chunkPos, ids.head))
}

object PhysCube {
  def apply(vel: V3F, pos: V3F, id: UUID, walk: V3F): PhysCube =
    PhysCube(vel, pos, id, walk, pos)
}