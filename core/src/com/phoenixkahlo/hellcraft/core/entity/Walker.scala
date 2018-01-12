package com.phoenixkahlo.hellcraft.core.entity

import com.phoenixkahlo.hellcraft.core
import com.phoenixkahlo.hellcraft.core.event.UE
import com.phoenixkahlo.hellcraft.core.util.ChunkMap
import com.phoenixkahlo.hellcraft.core.{CallService, Event, PutEnt, UpdateEffect}
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.service.PhysicsService
import com.phoenixkahlo.hellcraft.service.PhysicsService.{Act, Body, Capsule, Cylinder}

abstract class Walker[E <: Walker[E]] extends Entity[E] {
  this: E =>

  val rad: Float
  val capHeight: Float
  val springRest: Float
  val springSnap: Float
  val springConstMovingDown: Float
  val springConstMovingUp: Float = springConstMovingDown
  val springFriction: Float

  val mass: Float = 1
  val friction: Float = 1
  val inertia: Option[V3F] = None

  val pos: V3F
  val vel: V3F

  val walkForce: Float
  val walkSpeed: Float
  val walkDir: V2F

  def springEnabled: Boolean = true

  val rays: Seq[V3F] = Seq(Origin)

  def setPosVel(lastPos: V3F, newPos: V3F, newVel: V3F): E

  override def update = Walker.update(id)
}
object Walker {
  def update[E <: Walker[E]](id: EntID[E]): Seq[UpdateEffect] = {
    Seq(
      // event for performing pure physics
      Event(UE.ent(id).flatMap({
        case Some(ent) => UE.chunks(ent.chunkPos.neighbors).map(chunks => {
          // make a body
          val body = Body(Capsule(ent.rad, ent.capHeight), ent.pos, ent.vel, ent.mass, ent.friction, ent.inertia)
          val sprung: V3F =
            if (!ent.springEnabled) body.vel
            else {
              // apply spring force
              val grid = ChunkMap(chunks)
              // ray cast spring distances
              val springs: Seq[Float] = ent.rays
                .map(delta => body.pos + (Down * ent.capHeight / 2) + delta)
                .flatMap(point => grid.seghit(point, Down, ent.springSnap).map(_ dist point))
              // comp spring constant K
              val springConst =
                if (ent.vel.y > 0) ent.springConstMovingUp
                else ent.springConstMovingDown
              // compute the spring force
              var springForce = // upwards spring force magnitude
                if (springs.isEmpty) 0
                else {
                  val avg = springs.sum / springs.size
                  (ent.springRest + ent.capHeight / 2 - avg) * springConst
                }
              // apply friction to spring force
              val springForceFrictioned =
                if (springForce > 0) springForce - ent.springFriction
                else springForce + ent.springFriction
              if ((springForce > 0) == (springForceFrictioned > 0))
                springForce = springForceFrictioned
              else
                springForce = 0
              var sprung = body.vel + (Up * springForce * Delta.dtf / ent.mass)
              // don't allow spring force to overshoot
              if ((sprung.y > 0) ^ (body.vel.y > 0))
                sprung = sprung.copy(y = 0)
              sprung
            }
          // apply walking force
          var i = (ent.walkDir * ent.walkSpeed).inflate(sprung.y) - sprung // impulse to get velocity to walking velocity
          if (i.magnitude > ent.walkForce * Delta.dtf / ent.mass) // if impulse is more force than can provide
            i = i.normalize * ent.walkForce * Delta.dtf / ent.mass // set to max force that can provide
          val walked = sprung + i // apply impulse to velocity
          val replacement = ent.setPosVel(ent.pos, ent.pos, walked)
          Seq(
            // put new ent
            PutEnt(replacement),
            // invoke physics service
            CallService[PhysicsService, Body](Act(body.copy(vel = walked, pos = body.pos + (Down * ent.capHeight / 2))),
              (after: Body) => Seq(Event(UE.ent(id).map({
                case Some(ent) => Seq(PutEnt(ent.setPosVel(ent.pos, after.pos + (Up * ent.capHeight / 2), after.vel)))
                case None => Seq.empty
              }))))
          )
        })
        case None => UE(Seq.empty)
      }))

    )
  }
}