package com.phoenixkahlo.hellcraft.service

case class ServiceTag[S <: Service](ord: Int)
object ServiceTag {
  val tag: Seq[ServiceTag[_ <: Service]] =
    Seq(PhysicsTag)
  val total = tag.size
}