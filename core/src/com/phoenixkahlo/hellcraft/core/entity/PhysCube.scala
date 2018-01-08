package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.event.{Events, UE}
import com.phoenixkahlo.hellcraft.core.{Chunk, PutEnt, TerrainSoup, UpdateEffect}
import com.phoenixkahlo.hellcraft.fgraphics.PhysTID
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.math.physics.{ComplexCollider, EmptyBroadphase, MeshRequest, Triangle}

import scala.collection.mutable.ArrayBuffer
case class PhysCube(vel: V3F, override val pos: V3F, override val id: EntID[PhysCube], walk: V3F, override val lastPos: V3F) extends Cube[PhysCube] with Moveable[PhysCube] {
  override def updatePos(newPos: V3F): PhysCube = copy(pos = newPos, lastPos = pos)

  override def tid = PhysTID

  override def update: Seq[UpdateEffect] =
    Seq(Events({
      UE.chunks(chunkPos.neighbors).map(chunks => {
        val broadphase = chunks.map(_.broadphase).reduce(_ + _)
        val before = ComplexCollider(pos, vel + (Down * 9.8f * Delta.dtf), Repeated(0.5f), 1000, Delta.dtf, 100, walk)
        val after = before.update(broadphase)
        val replacement = copy(vel = after.vel, pos = after.pos, lastPos = pos)
        Seq(PutEnt(replacement))
      })
    }))
}

object PhysCube {
  def apply(vel: V3F, pos: V3F, walk: V3F)(implicit rand: MRNG): PhysCube =
    PhysCube(vel, pos, EntID(), walk, pos)
}