package com.phoenixkahlo.hellcraft.math.physics

import com.phoenixkahlo.hellcraft.math.V3F

case class ComplexCollider(pos: V3F, vel: V3F, rad: V3F, scale: Float, dt: Float) {


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
    simple = simple.update(eMeshes)

    // convert the velocity back into real time units
    var fVel = simple.vel.normalize * (simple.vel.magnitude / dt)
    // convert back into non-elliptical space
    var fPos = simple.pos ** sRad
    fVel = fVel ** sRad
    // convert back into non-scaled space
    fPos = fPos / scale
    fVel = fVel / scale

    // update the complex collider
    copy(
      pos = fPos,
      vel = fVel
    )
  }

}
