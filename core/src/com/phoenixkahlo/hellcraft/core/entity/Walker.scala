package com.phoenixkahlo.hellcraft.core.entity

import com.phoenixkahlo.hellcraft.core.event.UE
import com.phoenixkahlo.hellcraft.core.{CallService, ChunkMap, Event, PutEnt}
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.math.{Down, Origin, Up, V3F}
import com.phoenixkahlo.hellcraft.service.PhysicsService
import com.phoenixkahlo.hellcraft.service.PhysicsService.{Act, Body, Capsule}

abstract class Walker[E <: Walker[E]] extends Entity[E] {
  this: E =>

  val rad: Float
  val capHeight: Float
  val springRest: Float
  val springSnap: Float
  val springConst: Float

  val mass: Float = 1
  val friction: Float = 1
  val inertia: Option[V3F] = None

  val pos: V3F
  val vel: V3F

  val rays: Seq[V3F] = Seq(Origin)

  def setPosVel(lastPos: V3F, newPos: V3F, newVel: V3F): E

  override def update = {
    val body = Body(Capsule(rad, capHeight), pos, vel, mass, friction, inertia)
    Seq(CallService[PhysicsService, Body](Act(body), (body: Body) => {
      Seq(Event(UE.chunks(chunkPos.neighbors).map(chunks => {
        val grid = ChunkMap(chunks)
        val springs: Seq[Float] = rays
          .map(delta => body.pos + (Down * capHeight / 2) + delta)
          .flatMap(point => grid.seghit(point, Down, springSnap).map(_ dist point))
        val springForce = // upwards spring force
          if (springs.isEmpty) 0
          else {
            val avg = springs.sum / springs.size
            (springRest + capHeight / 2 - avg) * springConst
          }
        val sprung = body.vel + (Up * springForce * Delta.dtf)
        val replacement = setPosVel(pos, body.pos, sprung)
        Seq(PutEnt(replacement))
      })))
    }))
  }
}
