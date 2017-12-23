package com.phoenixkahlo.hellcraft.math.physics

import com.phoenixkahlo.hellcraft.math.{Origin, Repeated, V3F}
import com.phoenixkahlo.hellcraft.util.debugging.Profiler

case class MeshRequest(scale: Float, sRad: V3F)

case class ComplexCollider(pos: V3F, vel: V3F, rad: V3F, scale: Float, dt: Float, friction: Float, walk: V3F) {

  def update(broadphase: Broadphase): ComplexCollider = {
    val p = Profiler("complex collider")

    // convert to scaled space
    var ePos = pos * scale
    var eVel = vel * scale
    val sRad = rad * scale
    // convert to elliptical space
    ePos = ePos \\ sRad
    eVel = eVel \\ sRad
    // convert the velocity to delta time units
    eVel = eVel.normalize * (eVel.magnitude * dt)
    // convert the meshes to scaled, elliptical space
    val bRequest = BroadphaseRequest(pos, rad.magnitude + (vel * dt).magnitude)
    val mRequest = MeshRequest(scale, sRad)

    p.log()

    val eMeshes = Seq(broadphase(bRequest)(mRequest))

    p.log()

    // create the simple collider and update it to the final simple collider
    var simple = SimpleCollider(ePos, eVel)
    val (updated, normal) = simple.update(eMeshes)
    simple = updated

    p.log()

    // apply friction
    // natural velocity
    val natVel = ((walk * scale) \\ sRad) * dt
    // normal force over mass
    val nom = vel dot normal.neg
    // coefficient of friction, transformed into simple spacetime (which means it must become a vector)
    val cof = ((Repeated(friction) * scale) \\ sRad) * dt
    // change in velocity due to friction
    val fdv = (((simple.vel - natVel).normalize ** cof) * nom) * -1
    // apply friction, but don't overshoot
    var vaf = simple.vel + fdv
    if (((vaf - natVel) dot (simple.vel - natVel)) < 0)
      vaf = natVel
    // update the collider
    simple = simple.copy(vel = vaf)

    p.log()

    // convert the velocity back into real time units
    var fVel = simple.vel.normalize * (simple.vel.magnitude / dt)
    // convert back into non-elliptical space
    var fPos = simple.pos ** sRad
    fVel = fVel ** sRad
    // convert back into non-scaled space
    fPos = fPos / scale
    fVel = fVel / scale

    p.log()
    p.print()

    // update the complex collider
    copy(pos = fPos, vel = fVel, friction = 1, walk = Origin)
  }

}
