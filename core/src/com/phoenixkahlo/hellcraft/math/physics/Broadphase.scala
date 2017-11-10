package com.phoenixkahlo.hellcraft.math.physics

import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc
import com.phoenixkahlo.hellcraft.util.collections.spatial.Octree

case class BroadphaseRequest(center: V3F, radius: Float)

trait Broadphase {
  def apply(bRequest: BroadphaseRequest)(mRequest: MeshRequest): Seq[Triangle]

  def +(other: Broadphase): Broadphase
}

object EmptyBroadphase extends Broadphase {
  def apply(bRequest: BroadphaseRequest)(mRequest: MeshRequest): Seq[Nothing] = Seq.empty

  override def +(other: Broadphase): Broadphase = other
}

class BroadphaseCombination(a: Broadphase, b: Broadphase) extends Broadphase {
  def apply(bRequest: BroadphaseRequest)(mRequest: MeshRequest): Seq[Triangle] =
    a(bRequest)(mRequest) ++: b(bRequest)(mRequest)

  override def +(other: Broadphase) = new BroadphaseCombination(this, other)
}

class OctreeBroadphase(source: Iterator[Triangle], center: V3F, range: Float) extends Broadphase {
  /*
  val trees: ((Float, V3F)) => (Octree[Triangle], Float) = new MemoFunc[(Float, V3F), (Octree[Triangle], Float)]({
    case (scale, sRad) =>
      val triangles = source
        .map(_.map(p => (p * scale) \\ sRad))
        .foldLeft(Octree.empty[Triangle](
          (center * scale) \\ sRad,
          (range * scale) / sRad.monoidFold(Math.min))
        )((tree, tri) => tree + (tri.center -> tri))
      val maxDim =
        if (source.nonEmpty) source.map(_.maxDimension).max
        else 0
      (triangles, maxDim)
  })
  */
  val tree = source.foldLeft(Octree.empty[Triangle](center, range))((tree, tri) => tree + (tri.center -> tri))
  val maxDim =
    if (source nonEmpty) source.map(_.maxDimension).max
    else 0

  /*
  override def apply(request: MeshRequest): Seq[Triangle] = {
    val (tree, maxDim) = trees((request.scale, request.sRad))
    tree.within(request.rangeCenter, request.rangeRad + maxDim).map(_._2)
  }
  */
  override def apply(bRequest: BroadphaseRequest)(mRequest: MeshRequest) = {
    tree.within(bRequest.center, bRequest.radius + maxDim).map(_._2)
      .map(_.map(p => (p * mRequest.scale) \\ mRequest.sRad))
  }

  override def +(other: Broadphase) = new BroadphaseCombination(this, other)
}
