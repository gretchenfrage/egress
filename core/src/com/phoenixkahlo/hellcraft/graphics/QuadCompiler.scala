package com.phoenixkahlo.hellcraft.graphics


import com.badlogic.gdx.graphics.Color
import com.phoenixkahlo.hellcraft.math.Quad

import scala.collection.mutable.ArrayBuffer

object QuadCompiler {

  def apply(quads: Seq[Quad], resources: ResourcePack): (Array[Float], Array[Short]) = {
    val tris = quads.flatMap(_.decompose).flatMap(_.bothSides)

    val verts = new ArrayBuffer[Float]
    val indices = new ArrayBuffer[Short]

    val (u1, v1) = (resources.apply(StoneTID).getU, resources.apply(StoneTID).getV)
    val (u2, v2) = (resources.apply(StoneTID).getU2, resources.apply(StoneTID).getV)
    val (u3, v3) = (resources.apply(StoneTID).getU2, resources.apply(StoneTID).getV2)

    for (i <- tris.indices) {
      val tri = tris(i)
      indices.append((i * 3).toShort, (i * 3 + 1).toShort, (i * 3 + 2).toShort)
      verts.append(
        tri.a.x, tri.a.y, tri.a.z, Color.WHITE.toFloatBits, u1, v1,
        tri.b.x, tri.b.y, tri.b.z, Color.WHITE.toFloatBits, u2, v2,
        tri.c.x, tri.c.y, tri.c.z, Color.WHITE.toFloatBits, u3, v3
      )
    }

    (verts.toArray, indices.toArray)
  }

}
