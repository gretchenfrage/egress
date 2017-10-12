package com.phoenixkahlo.hellcraft.graphics.models

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.{Color, Mesh, VertexAttribute}
import com.phoenixkahlo.hellcraft.graphics.{ResourcePack, SheetTextureID}
import com.phoenixkahlo.hellcraft.util.caches.KeyParamPool

object FreeCubeMesh extends KeyParamPool[SheetTextureID, (SheetTextureID, ResourcePack), Mesh]({ case (tid, pack) => {
  val n = -0.5f
  val p = 0.5f
  val col = Color.WHITE.toFloatBits
  val tex = pack.sheetRegion(tid)
  val verts = Array[Float](
    // south face
    n, n, n,   col,    tex.getU2, tex.getV,    0, 0, -1,  // 0
    p, n, n,   col,    tex.getU, tex.getV,     0, 0, -1,  // 1
    p, p, n,   col,    tex.getU, tex.getV2,     0, 0, -1,  // 2
    n, p, n,   col,    tex.getU2, tex.getV2,   0, 0, -1,  // 3
    // north face
    n, n, p,   col,    tex.getU, tex.getV,     0, 0, 1,   // 4
    p, n, p,   col,    tex.getU2, tex.getV,    0, 0, 1,   // 5
    p, p, p,   col,    tex.getU2, tex.getV2,   0, 0, 1,   // 6
    n, p, p,   col,    tex.getU,  tex.getV2,   0, 0, 1,   // 7
    // west face
    p, n, n,   col,    tex.getU2, tex.getV,    1, 0, 0,   // 8
    p, n, p,   col,    tex.getU, tex.getV,     1, 0, 0,   // 9
    p, p, p,   col,    tex.getU, tex.getV2,    1, 0, 0,   // 10
    p, p, n,   col,    tex.getU2, tex.getV2,   1, 0, 0,   // 11
    // east face
    n, n, n,   col,    tex.getU, tex.getV,     -1, 0, 0,  // 12
    n, n, p,   col,    tex.getU2, tex.getV,    -1, 0, 0,  // 13
    n, p, p,   col,    tex.getU2, tex.getV2,   -1, 0, 0,  // 14
    n, p, n,   col,    tex.getU, tex.getV2,    -1, 0, 0,  // 15
    // up face
    n, p, n,   col,    tex.getU2, tex.getV,    0, 0, 1,   // 16
    p, p, n,   col,    tex.getU, tex.getV,     0, 0, 1,   // 17
    p, p, p,   col,    tex.getU, tex.getV2,    0, 0, 1,   // 18
    n, p, p,   col,    tex.getU2, tex.getV2,   0, 0, 1,   // 19
    // down face
    n, n, n,   col,    tex.getU2, tex.getV2,   0, 0, -1,  // 20
    p, n, n,   col,    tex.getU, tex.getV2,    0, 0, -1,  // 21
    p, n, p,   col,    tex.getU, tex.getV,     0, 0, -1,  // 22
    n, n, p,   col,    tex.getU2, tex.getV,    0, 0, -1   // 23
  )
  val indices = Array[Short](
    // south face
    2, 1, 0,      3, 2, 0,
    // north face
    5, 6, 4,      6, 7, 4,
    // west face
    10, 9, 8,     11, 10, 8,
    // east face
    13, 14, 12,   14, 15, 12,
    // up face
    18, 17, 16,   19, 18, 16,
    // down face
    21, 22, 20,   22, 23, 20
  )

  val mesh = new Mesh(true, verts.length, indices.length,
    new VertexAttribute(Usage.Position, 3, "a_position"),
    new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
    new VertexAttribute(Usage.TextureCoordinates, 2, "a_texCoord0"),
    new VertexAttribute(Usage.Normal, 3, "a_normal")
  )

  mesh.setVertices(verts)
  mesh.setIndices(indices)

  mesh
}})
