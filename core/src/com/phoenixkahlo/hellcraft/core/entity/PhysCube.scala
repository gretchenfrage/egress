package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.carbonite.CarboniteFields
import com.phoenixkahlo.hellcraft.core.{DoPhysics, PutEntity, Shift, TerrainSoup, UpdateEffect, World}
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.graphics.{GrassTID, PhysTID}
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.math.physics.{ComplexCollider, Triangle}

import scala.collection.mutable.ArrayBuffer

@CarboniteFields
case class PhysCube(vel: V3F, override val pos: V3F, override val id: UUID) extends Cube(PhysTID, pos, id) with Moveable {
  override def updatePos(newPos: V3F): Entity = copy(pos = newPos)

  def doPhysics(world: World): PhysCube = {
    val meshes: Seq[Seq[Triangle]] =
      chunkPos.neighbors.flatMap(world.chunkAt).flatMap(_.terrainSoup).map(
        _.iterator.map({ case (p1, p2, p3) => Triangle(p1, p2, p3) }).toSeq)

    var collider = ComplexCollider(pos, vel + (Down * Delta.dtf), Repeated(0.5f), 1000, Delta.dtf, 1)
    collider = collider.update(meshes)

    copy(
      pos = collider.pos,
      vel = collider.vel
    )
  }

  override def update(world: World, ids: Stream[UUID]): Seq[UpdateEffect] =
    Seq(DoPhysics(id, chunkPos, ids.head))
}