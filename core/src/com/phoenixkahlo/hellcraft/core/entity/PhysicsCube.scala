package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.carbonite.CarboniteFields
import com.phoenixkahlo.hellcraft.core.{PutEntity, Shift, TerrainSoup, UpdateEffect, World}
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.graphics.{GrassTID, PhysTID}
import com.phoenixkahlo.hellcraft.math._

import scala.collection.mutable.ArrayBuffer

object Physics {

  def checkPointInTriangle(p: V3F, a: V3F, b: V3F, c: V3F): Boolean = {
    def sameSide(p1: V3F, p2: V3F, a: V3F, b: V3F): Boolean = {
      val cp1 = (b - a) cross (p1 - a)
      val cp2 = (b - a) cross (p2 - a)
      (cp1 dot cp2) >= 0
    }
    sameSide(p, a, b, c) && sameSide(p, b, a, c) && sameSide(p, c, a, b)
  }

  def getLowestRoot(a: Float, b: Float, c: Float, max: Float): Option[Float] = {
    val determinant = b * b - 4 * a * c
    if (determinant < 0) None
    else {
      val sqrtd = Math.sqrt(determinant).toFloat
      val r1 = (-b - sqrtd) / (2 * a)
      val r2 = (-b + sqrtd) / (2 * a)
      Some(Math.min(r1, r2)).filter(_ > 0).filter(_ < max)
    }
  }

  case class Collision(dist: Float, point: V3F)
  def checkTriangle(xi: V3F, dx: V3F, p1: V3F, p2: V3F, p3: V3F): Option[Collision] = {
    val plane = Plane(p1, p2, p3)
    if (plane frontFacingTo dx.normalize) {
      {
        val range: Option[(Float, Float)] = {
          val signedDist = plane signedDistanceTo xi
          val norDotVel = plane.nor dot dx
          if (norDotVel == 0) {
            if (Math.abs(signedDist) >= 1) return None
            else None
          } else {
            val a = (-1 - signedDist) / norDotVel
            val b = (+1 - signedDist) / norDotVel
            val t0 = Math.min(a, b)
            val t1 = Math.max(a, b)
            if (t0 > 1 || t1 < 0) return None
            else Some((Trig.clamp(t0, 0, 1), Trig.clamp(t1, 0, 1)))
          }
        }

        if (range isDefined) {
          val (t0, t1) = range.get
          val point = (xi - plane.nor) + (dx * t0)
          if (checkPointInTriangle(point, p1, p2, p3)) {
            println("face collision")
            val dist = t0 * dx.magnitude
            return Some(Collision(dist, point))
          }
        }
      }

      // check for edge and vertex collisions
      val velMagSqr = dx.magnitudeSqrd

      var t = 0f
      var found = false
      var point: V3F = Origin
      var message: String = ""

      // vertex collisions
      for (p <- Seq(p1, p2, p3)) {
        val a = velMagSqr
        val b = 2 * (dx dot (xi - p))
        val c = (p - xi).magnitudeSqrd - 1
        for (root <- getLowestRoot(a, b, c, t)) {
          t = root
          point = p
          found =  true
          message = "vertex collision"
        }
      }

      // edge collisions
      for ((e1, e2) <- Seq(p1 -> p2, p2 -> p3, p3 -> p1)) {
        val edge = e2 - p1
        val xiToVert = e1 - xi

        val edgeMagSqrd = edge.magnitudeSqrd
        val edgeDotDX = edge dot dx
        val edgeDotXIToVert = edge dot xiToVert

        val a = edgeMagSqrd * -velMagSqr + edgeDotDX * edgeDotDX
        val b = edgeMagSqrd * (2 * (dx dot xiToVert)) - 2 * edgeDotDX * edgeDotXIToVert
        val c = edgeMagSqrd * (1 - xiToVert.magnitudeSqrd) + edgeDotXIToVert * edgeDotXIToVert

        for (root <- getLowestRoot(a, b, c, t)) {
          val f = (edgeDotDX * root - edgeDotXIToVert) / edgeMagSqrd
          if (f >= 0 && f <= 1) {
            t = root
            found = true
            point = e1 + (edge * f)
            message = "edge collision"
          }
        }
      }

      if (found) {
        println(message)
        val dist = t * dx.magnitude
        Some(Collision(dist, point))
      } else None
    } else None
  }
}

@CarboniteFields
case class PhysCube(vel: V3F, override val pos: V3F, override val id: UUID) extends Cube(PhysTID, pos, id) with Moveable {
  override def updatePos(newPos: V3F): Entity = copy(pos = newPos)

  override def update(world: World, ids: Stream[UUID]): Seq[UpdateEffect] = {
    val collisions: Seq[Physics.Collision] =
      chunkPos.neighbors.flatMap(world.chunkAt).flatMap(_.terrainSoup).flatMap(_.iterator)
        .flatMap({ case (p1, p2, p3) => Physics.checkTriangle(pos, vel * Delta.dtf, p1, p2, p3) })

    if (collisions nonEmpty)
      println(collisions)

    Seq(Shift(vel * Delta.dtf, id, chunkPos, ids.head)) ++
      collisions.sortBy(_.dist).headOption.map(c => PutEntity(new Cube(GrassTID, c.point, ids.drop(1).head), ids.drop(1).head))
  }
}

/*
object CheckPointInTriangle {
  def sameSide(p1: V3F, p2: V3F, a: V3F, b: V3F): Boolean = {
    val cp1 = (b - a) cross (p1 - a)
    val cp2 = (b - a) cross (p2 - a)
    (cp1 dot cp2) >= 0
  }

  def apply(p: V3F, a: V3F, b: V3F, c: V3F): Boolean = {
    sameSide(p, a, b, c) && sameSide(p, b, a, c) && sameSide(p, c, a, b)
  }
}

object GetLowestRoot {
  def apply(a: Float, b: Float, c: Float, maxR: Float): Option[Float] = {
    val determinant = b * b - 4 * a * c

    if (determinant < 0) None
    else {
      val sqrtD = Math.sqrt(determinant).toFloat
      var r1 = (-b - sqrtD) / (2 * a)
      var r2 = (-b + sqrtD) / (2 * a)

      if (r1 > r2) {
        val temp = r2
        r2 = r1
        r1 = temp
      }

      if (r1 > 0 && r1 < maxR) Some(r1)
      else if (r2 > 0 && r2 < maxR) Some(r2)
      else None
    }
  }
}
*/

/*
@CarboniteFields
case class PhysCube(vel: V3F, override val pos: V3F, override val id: UUID) extends Cube(PhysTID, pos, id) with Moveable {
  override def updatePos(newPos: V3F): Entity = copy(pos = newPos)

  override def update(world: World, ids: Stream[UUID]): Seq[UpdateEffect] = {
    // check for collision
    object colPackage {
      var foundCollision = false
      var nearestDistance = 0f
      var intersectionPoint: V3F = Origin
    }
    def checkTriangle(p1: V3F, p2: V3F, p3: V3F): Unit = {
      // make the plane containing this triangle
      val trianglePlane = Plane(p1, p2, p3)

      // is triangle front-facing to the velocity vector?
      if (trianglePlane frontFacingTo vel.normalize) {
        // get interval of plane intersection
        var t0 = 0f
        var t1 = 0f
        var embeddedInPlane = false

        // calculate the signed distance from sphere position to triangle plane
        val signedDistToTrianglePlane = trianglePlane signedDistanceTo pos

        // cache this as we're going to use it a few times below
        val normalDotVel = trianglePlane.nor dot vel

        // if sphere is travelling parallel to the plane
        if (normalDotVel == 0) {
          if (Math.abs(signedDistToTrianglePlane) >= 1) {
            // sphere is not embedded in plane, no collision possible
            return
          } else {
            // sphere is embedded in plane. it intersects in the whole range [0..1]
            embeddedInPlane = true
            t0 = 0
            t1 = 1
          }
        } else {
          // N dot D is not 0. calculate intersection interval
          t0 = (-1 - signedDistToTrianglePlane) / normalDotVel
          t1 = (+1 - signedDistToTrianglePlane) / normalDotVel

          // swap so t0 < t1
          if (t0 > t1) {
            val temp = t1
            t1 = t0
            t0 = temp
          }

          // check that at least one result is within range
          if (t0 > 1 || t1 < 0) {
            // both t values are outside values [0, 1]. no collision possible
            return
          } else {
            // clamp to [0, 1]
            if (t0 < 0) t0 = 0
            if (t1 < 0) t1 = 0
            if (t0 > 1) t0 = 1
            if (t1 > 1) t1 = 1
          }
        }

        // OK, at this point we have two time values t0 and t1
        // between which the swept sphere intersects with the
        // triangle plane. If any collision is to occur it must
        // happen within this interval.
        var collisionPoint: V3F = Origin
        var foundCollision = false
        var t = 1f

        // First we check for the easy case - collision inside
        // the triangle. If this happens it must be at time t0
        // as this is when the sphere rests on the front side
        // of the triangle plane. Note, this can only happen if
        // the sphere is not embedded in the triangle plane.
        if (!embeddedInPlane) {
          val planeIntersectionPoint = (pos - trianglePlane.nor) + (vel * t0)

          if (CheckPointInTriangle(planeIntersectionPoint, p1, p2, p3)) {
            foundCollision = true
            t = t0
            collisionPoint = planeIntersectionPoint
          }
        }

        // if we haven’t found a collision already we’ll have to
        // sweep sphere against points and edges of the triangle.
        // Note: A collision inside the triangle (the check above)
        // will always happen before a vertex or edge collision!
        // This is why we can skip the swept test if the above
        // gives a collision!
        if (!foundCollision) {
          val velSquaredLen = vel.magnitudeSqrd
          var a = 0f
          var b = 0f
          var c = 0f
          var newT = 0f

          // For each vertex or edge a quadratic equation have to
          // be solved. We parameterize this equation as
          // a*t^2 + b*t + c = 0 and below we calculate the
          // parameters a,b and c for each test.

          // Check against points:
          a = velSquaredLen

          for (p <- Seq(p1, p2, p3)) {
            b = 2 * (vel dot (pos - p))
            val c = (p - pos).magnitudeSqrd - 1
            for (root <- GetLowestRoot(a, b, c, t)) {
              newT = root
              t = newT
              foundCollision = true
              collisionPoint = p2
            }
          }

          // Check against edges:
          // p1 -> p2:
          var edge = p2 - p1
          var baseToVertex = p1 - pos
          var edgeSqrdLen = edge.magnitudeSqrd
          var edgeDotVel = edge dot vel
          var edgeDotBaseToVertex = edge dot baseToVertex

          // calculate parameters for equation
          a = edgeSqrdLen * -velSquaredLen + edgeDotVel * edgeDotVel
          b = edgeSqrdLen * (2 * vel.dot(baseToVertex)) - 2 * edgeDotVel * edgeDotBaseToVertex
          c = edgeSqrdLen * (1 - baseToVertex.magnitudeSqrd) + edgeDotBaseToVertex * edgeDotBaseToVertex

          // does the swept sphere collide against infinite edge?
          for (root <- GetLowestRoot(a, b, c, t)) {
            newT = root

            // check if intersection is within line segment
            val f = (edgeDotVel * newT - edgeDotBaseToVertex) / edgeSqrdLen
            if (f >= 0 && f <= 1) {
              // intersection took place within segment
              t = newT
              foundCollision = true
              collisionPoint = p1 + (edge * f)
            }
          }

          // p2 -> p3
          edge = p3 - p2
          baseToVertex = p2 - pos
          edgeSqrdLen = edge.magnitudeSqrd
          edgeDotVel = edge dot vel
          edgeDotBaseToVertex = edge dot baseToVertex

          a = edgeSqrdLen * -velSquaredLen + edgeDotVel * edgeDotVel
          b = edgeSqrdLen * (2 * vel.dot(baseToVertex)) - 2 * edgeDotVel * edgeDotBaseToVertex
          c = edgeSqrdLen * (1 - baseToVertex.magnitudeSqrd) + edgeDotBaseToVertex * edgeDotBaseToVertex

          for (root <- GetLowestRoot(a, b, c, t)) {
            newT = root
            val f = (edgeDotVel * newT - edgeDotBaseToVertex) / edgeSqrdLen
            if (f >= 0 && f < 1) {
              t = newT
              foundCollision = true
              collisionPoint = p2 + (edge * f)
            }
          }

          // p3 -> p1
          edge = p1 - p3
          baseToVertex = p3 - pos
          edgeSqrdLen = edge.magnitudeSqrd
          edgeDotVel = edge dot vel
          edgeDotBaseToVertex = edge dot baseToVertex

          a = edgeSqrdLen * -velSquaredLen + edgeDotVel * edgeDotVel
          b = edgeSqrdLen * (2 * vel.dot(baseToVertex)) - 2 * edgeDotVel * edgeDotBaseToVertex
          c = edgeSqrdLen * (1 - baseToVertex.magnitudeSqrd) + edgeDotBaseToVertex * edgeDotBaseToVertex

          for (root <- GetLowestRoot(a, b, c, t)) {
            newT = root
            val f = (edgeDotVel * newT - edgeDotBaseToVertex) / edgeSqrdLen
            if (f >= 0 && f <= 1) {
              t = newT
              foundCollision = true
              collisionPoint = p3 + (edge * f)
            }
          }
        }
        // set result
        if (foundCollision) {
          // distance to collision - t is time of collision
          val distToCollision = t * vel.magnitude

          // does this triangle quialify for the closest hit?
          // it does if it's the first hit or the closest
          if (!colPackage.foundCollision || distToCollision < colPackage.nearestDistance) {
            colPackage.nearestDistance = distToCollision
            colPackage.intersectionPoint = collisionPoint
            colPackage.foundCollision = true
          }
        }
      }
    }

    if (vel != Origin) {
      val triangles: Seq[(V3F, V3F, V3F)] = chunkPos.neighbors.flatMap(world.chunkAt).flatMap(_.terrainSoup).flatMap(_.iterator)
      for ((p1, p2, p3) <- triangles) {
        checkTriangle(p1, p2, p3)
      }
    }

    var events = new ArrayBuffer[UpdateEffect]
    if (colPackage.foundCollision) {
      events += PutEntity(new Cube(GrassTID, colPackage.intersectionPoint, ids.head), ids.drop(1).head)
    }
    events += Shift(vel, id, chunkPos, ids.drop(2).head)

    events
  }
}

object PhysCube {
  val gravity = Down * 9.8f * Delta.dtf
}
*/