package com.phoenixkahlo.hellcraft.math.physics

import com.phoenixkahlo.hellcraft.math.V3F

case class SimpleCollider(pos: V3F, vel: V3F) {

  def update(meshes: Seq[Seq[Triangle]]): SimpleCollider = {
    val veryCloseDistance = 0.005f

    def collideWithWorld(pos: V3F, vel: V3F, depth: Int): (V3F, V3F) = {
      if (depth == 0) (pos, vel)
      else {
        val sweep = SweptSphere(pos, vel)
        meshes.flatMap(sweep.collideMesh).sortBy(_.distance).headOption match {
          case Some(collision) =>
            var intersection = collision.intersection
            val distance = collision.distance

            val destinationPoint = pos + vel
            var newBasePoint = pos

            if (distance >= veryCloseDistance) {
              newBasePoint = pos + (vel.normalize * (distance - veryCloseDistance))
              intersection -= vel.normalize * veryCloseDistance
            }

            val slidePlaneOrigin = intersection
            val slidePlaneNormal = (newBasePoint - intersection).normalize
            val slidePlane = Plane(slidePlaneOrigin, slidePlaneNormal)

            val newDestinationPoint = destinationPoint - (slidePlaneNormal * slidePlane.signedDistanceTo(destinationPoint).toFloat)

            val newVelocityVector = newDestinationPoint - intersection

            if (newVelocityVector.magnitude < veryCloseDistance)
              (newBasePoint, vel)
            else
              collideWithWorld(newBasePoint, newVelocityVector, depth - 1)
          case None => (pos + vel, vel)
        }
      }
    }

    val (finalPos, finalVel) = collideWithWorld(pos, vel, 5)
    SimpleCollider(finalPos, finalVel)
  }

}
