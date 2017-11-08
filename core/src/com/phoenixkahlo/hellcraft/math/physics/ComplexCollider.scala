package com.phoenixkahlo.hellcraft.math.physics

import com.phoenixkahlo.hellcraft.math.{Origin, Repeated, V3F}

case class ComplexCollider(pos: V3F, vel: V3F, rad: V3F, scale: Float, dt: Float, friction: Float) {


  def update(meshes: Seq[Seq[Triangle]]): ComplexCollider = {
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
    val eMeshes = meshes.map(_.map(_.map(p => (p * scale) \\ sRad)))

    // create the simple collider and update it to the final simple collider
    var simple = SimpleCollider(ePos, eVel)
    val (updated, normal) = simple.update(eMeshes)
    simple = updated

    // apply friction
    // normal force over mass
    val nom = vel dot normal.neg
    // coefficient of friction, transformed into simple spacetime (which means it must become a vector)
    val cof = ((Repeated(friction) * scale) \\ sRad) * dt
    // change in velocity due to friction
    val fdv = ((simple.vel.normalize ** cof) * nom) * -1
    // apply friction, but don't overshoot
    var vaf = simple.vel + fdv
    if ((vaf dot simple.vel) < 0)
      vaf = Origin
    // update the collider
    simple = simple.copy(vel = vaf)

    // convert the velocity back into real time units
    var fVel = simple.vel.tryNormalize * (simple.vel.magnitude / dt)
    // convert back into non-elliptical space
    var fPos = simple.pos ** sRad
    fVel = fVel ** sRad
    // convert back into non-scaled space
    fPos = fPos / scale
    fVel = fVel / scale

    // update the complex collider
    copy(pos = fPos, vel = fVel, friction = 1)
  }

}
