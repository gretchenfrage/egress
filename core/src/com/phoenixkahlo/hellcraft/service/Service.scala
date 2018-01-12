package com.phoenixkahlo.hellcraft.service

import com.phoenixkahlo.hellcraft.math.V3F

trait Service {
  type Call[T]
}

trait PhysicsService extends Service {
  override type Call[T] = PhysicsService.Call[T]
}
object PhysicsService {
  sealed trait Call[T]
  case class Act(body: Body) extends Call[Body]

  sealed trait BodyShape
  case class Sphere(rad: Float) extends BodyShape
  case class Capsule(rad: Float, height: Float) extends BodyShape
  case class Cylinder(rad: Float, height: Float) extends BodyShape

  case class Body(shape: BodyShape, pos: V3F, vel: V3F, mass: Float, friction: Float, inertia: Option[V3F] = None)
}