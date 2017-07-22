package com.phoenixkahlo.hellcraft.math

sealed trait AxialPlane

case class XYPlane(z: Float) extends AxialPlane
case class YZPlane(x: Float) extends AxialPlane
case class XZPlane(y: Float) extends AxialPlane