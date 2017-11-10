package com.phoenixkahlo.hellcraft.math.physics

import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc
import com.phoenixkahlo.hellcraft.util.collections.spatial.Octree

case class MeshRequest(scale: Float, sRad: V3F, rangeCenter: V3F, rangeRad: Float)

trait Broadphase {
  def apply(request: MeshRequest): Seq[Triangle]

  def +(other: Broadphase): Broadphase
}

object EmptyBroadphase extends Broadphase {
  override def apply(request: MeshRequest): Seq[Nothing] = Seq.empty

  override def +(other: Broadphase): Broadphase = other
}

class BroadphaseCombination(a: Broadphase, b: Broadphase) extends Broadphase {
  override def apply(request: MeshRequest): Seq[Triangle] = a(request) ++: b(request)

  override def +(other: Broadphase) = new BroadphaseCombination(this, other)
}

class OctreeMemoBroadphase(source: Iterator[Triangle], center: V3F, range: Float) extends Broadphase {
  val trees: ((Float, V3F)) => (Octree[Triangle], Float) = new MemoFunc[(Float, V3F), (Octree[Triangle], Float)]({
    case (scale, sRad) =>
      (
        source
          .map(_.map(p => (p * scale) \\ sRad))
          .foldLeft(Octree.empty[Triangle](center, range))((tree, tri) => tree + (tri.center -> tri)),
        source.map(_.maxDimension).max
      )
  })

  override def apply(request: MeshRequest): Seq[Triangle] = {
    val (tree, maxDim) = trees((request.scale, request.sRad))
    tree.within(request.rangeCenter, request.rangeRad + maxDim).map(_._2)
  }

  override def +(other: Broadphase) = new BroadphaseCombination(this, other)
}
