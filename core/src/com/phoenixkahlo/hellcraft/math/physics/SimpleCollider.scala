package com.phoenixkahlo.hellcraft.math.physics

import com.phoenixkahlo.hellcraft.math.{Origin, V3F}

case class SimpleCollider(pos: V3F, vel: V3F) {

  def update(meshes: Seq[Seq[Triangle]]): (SimpleCollider, V3F) = {
    val veryCloseDistance = 0.005f

    def collideWithWorld(pos: V3F, vel: V3F, depth: Int, normals: List[(V3F, Float)]): (V3F, V3F, List[(V3F, Float)]) = {
      if (depth == 0) (pos, vel, normals)
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

            var newVelocityVector = newDestinationPoint - intersection

            val timeInterval = distance / vel.magnitude
            val newNormals = (slidePlaneNormal -> timeInterval) :: normals

            if (newVelocityVector.magnitude < veryCloseDistance)
              (newBasePoint, vel, newNormals)
            else
              collideWithWorld(newBasePoint, newVelocityVector, depth - 1, newNormals)
          case None => (pos + vel, vel, normals)
        }
      }
    }

    val (finalPos, finalVel, normals) = collideWithWorld(pos, vel, 5, Nil)
    val avgNormal = normals.foldLeft[V3F](Origin)({ case (accum, (neu, weight)) => accum + (neu * weight) }).tryNormalize
    (SimpleCollider(finalPos, finalVel), avgNormal)
  }

}
