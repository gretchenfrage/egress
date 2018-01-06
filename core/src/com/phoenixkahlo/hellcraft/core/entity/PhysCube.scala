package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.{ChunkEvent, TerrainSoup, UpdateEffect, World}
import com.phoenixkahlo.hellcraft.fgraphics.PhysTID
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.math.physics.{ComplexCollider, EmptyBroadphase, MeshRequest, Triangle}

import scala.collection.mutable.ArrayBuffer

case class PhysCube(vel: V3F, override val pos: V3F, override val id: EntID[PhysCube], walk: V3F, override val lastPos: V3F) extends Cube[PhysCube](PhysTID, pos) with Moveable[PhysCube] {
  override def updatePos(newPos: V3F): PhysCube = copy(pos = newPos, lastPos = pos)

  def doPhysics(world: World): PhysCube = {
    val broadphase = chunkPos.neighbors.flatMap(world.chunkAt).map(_.broadphase).reduceOption(_ + _).getOrElse(EmptyBroadphase)

    var collider = ComplexCollider(pos, vel + (Down * 9.8f * Delta.dtf), Repeated(0.5f), 1000, Delta.dtf, 100, walk)
    collider = collider.update(broadphase)

    copy(vel = collider.vel, pos = collider.pos, lastPos = pos)
  }

  override def update(world: World)(implicit rand: MRNG): Seq[UpdateEffect] =
    Seq(ChunkEvent.physics(chunkPos, id))
}

object PhysCube {
  def apply(vel: V3F, pos: V3F, walk: V3F)(implicit rand: MRNG): PhysCube =
    PhysCube(vel, pos, EntID(), walk, pos)
}