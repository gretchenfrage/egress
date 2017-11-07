package com.phoenixkahlo.hellcraft.math.physics

import com.phoenixkahlo.hellcraft.math.{Trig, V3F}

case class Collision(intersection: V3F, distance: Float)

case class SweptSphere(xi: V3F, dx: V3F) {

  def collideMesh(mesh: Seq[Triangle]): Option[Collision] = {
    mesh.flatMap(collide).sortBy(_.distance).headOption
  }

  def collide(tri: Triangle): Option[Collision] = {
    // find the interval of plane intersection, and whether it's embedded in the plane
    val interval: Option[(Double, Double, Boolean)] =
      if ((tri.plane.normal dot dx) != 0) {
        // regular case
        Some((
          (+1 - tri.plane.signedDistanceTo(xi)) / (tri.plane.normal dot dx),
          (-1 - tri.plane.signedDistanceTo(xi)) / (tri.plane.normal dot dx),
          false
        ))
      } else {
        // velocity vector is perpendicular to plane normal
        if (tri.plane.absDistanceTo(xi) < 1) {
          // sphere is embedded in triangle plane
          Some((0, 1, true))
        } else {
          // sphere never collides
          None
        }
      }
    // return now if no interval was found
    if (interval.isEmpty)
      return None
    // otherwise, unpack and continue
    var (t0, t1, embedded) = interval.get
    // sort the interval of plane intersection
    if (t0 > t1) {
      val temp = t0
      t0 = t1
      t1 = temp
    }
    // if both values are outside of [0, 1], there cannot be a collision
    if (t0 > 1 || t1 < 0)
      return None
    // clamp the range
    t0 = Trig.clamp(t0, 0, 1)
    t1 = Trig.clamp(t1, 0, 1)

    // check for a collision with the inside of the triangle
    val insideCollision: Option[Collision] =
      if (embedded) None // an embedded sphere cannot collide with the inside
      else {
        val point = xi - tri.plane.normal + (dx * t0.toFloat)
        if (tri contains point) Some(Collision(point, t0 * dx.magnitude toFloat))
        else None
      }

    // if it collides with the face, we can return that collision
    // otherwise, we will have to check for collision with edges and vertices
    if (insideCollision.isDefined) insideCollision
    else {
      // cache some things
      val velocitySquaredLength = dx.magnitudeSqrd

      // collide with a vertex
      def vertCollision(vert: V3F): Option[Collision] = {
        val a = velocitySquaredLength
        val b = 2 * (dx dot (xi - vert))
        val c = (vert - xi).magnitudeSqrd - 1
        Quadratic.getLowestRoot(a, b, c).filter(_ < 1).map(t => Collision(vert, t * dx.magnitude))
      }

      // collide with an edge
      def edgeCollision(p1: V3F, p2: V3F): Option[Collision] = {
        val edge = p2 - p1
        val baseToVertex = p1 - xi
        val edgeSquaredLength = edge.magnitudeSqrd
        val edgeDotVel = edge dot dx
        val edgeDotBaseToVertex = edge dot baseToVertex
        val a = edgeSquaredLength * -velocitySquaredLength + edgeDotVel * edgeDotVel
        val b = edgeSquaredLength * (2 * (dx dot baseToVertex)) - 2 * edgeDotVel * edgeDotBaseToVertex
        val c = edgeSquaredLength * (1 - baseToVertex.magnitudeSqrd) + edgeDotBaseToVertex * edgeDotBaseToVertex
        Quadratic.getLowestRoot(a, b, c).filter(_ < 1).flatMap(t => {
          val f = (edgeDotVel * t - edgeDotBaseToVertex) / edgeSquaredLength
          if (f >= 0 && f <= 1) Some(Collision(p1 + (edge * f), t * dx.magnitude))
          else None
        })
      }

      // return the optional best collision with all vertices and edges
      Seq(
        vertCollision(tri.p1), vertCollision(tri.p2), vertCollision(tri.p3),
        edgeCollision(tri.p1, tri.p2), edgeCollision(tri.p2, tri.p3), edgeCollision(tri.p3, tri.p1)
      ).flatten.sortBy(_.distance).headOption
    }
  }

}
