package com.phoenixkahlo.hellcraft.math

case class ParamLine3D(p: V3F, d: V3F) {

  def intersection(plane: AxialPlane): Option[V3F] = plane match {
    case XYPlane(z) => V3F.tryMake(
      (z * d.x - p.z * d.x + p.x * d.z) / d.z,
      (z * d.y - p.z * d.y + p.y * d.z) / d.z,
      z
    )
    case YZPlane(x) => V3F.tryMake(
      x,
      (x * d.y - p.x * d.y + p.y * d.x) / d.x,
      (x * d.z - p.x * d.z + p.z * d.x) / d.x
    )
    case XZPlane(y) => V3F.tryMake(
      (y * d.x - p.y * d.x + p.x * d.y) / d.y,
      y,
      (y * d.z - p.y * d.z + p.z * d.y) / d.y
    )
  }

}
