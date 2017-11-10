package com.phoenixkahlo.hellcraft.math.physics

import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc
import com.phoenixkahlo.hellcraft.util.collections.spatial.Octree

import scala.collection.mutable.ArrayBuffer

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
  val triangles = source.to[ArrayBuffer]
  val tree = triangles.foldLeft(Octree.empty[Triangle](center, range))((tree, tri) => tree + (tri.center -> tri))
  val maxDim =
    if (triangles nonEmpty) triangles.map(_.maxDimension).max
    else 0

  override def apply(bRequest: BroadphaseRequest)(mRequest: MeshRequest) = {
    tree.within(bRequest.center, bRequest.radius + maxDim / 2).map(_._2)
      .map(_.map(p => (p * mRequest.scale) \\ mRequest.sRad))
  }

  override def +(other: Broadphase) = new BroadphaseCombination(this, other)
}
