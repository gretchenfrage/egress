package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.{Color, Mesh, VertexAttribute}
import com.badlogic.gdx.graphics.g3d.Model
import com.phoenixkahlo.hellcraft.util.caches.KeyParamPool

object FreeCubeMesh extends KeyParamPool[SheetTextureID, (SheetTextureID, ResourcePack), Mesh]({ case (tid, pack) => {
  val n = -0.5f
  val p = 0.5f
  val col = Color.WHITE.toFloatBits
  val tex = pack.sheetRegion(tid)
  val verts = Array[Float](
    // south face
    n, n, n,   col,    tex.getU2, tex.getV,    0, 0, -1,
    p, n, n,   col,    tex.getU, tex.getV,     0, 0, -1,
    p, p, n,   col,    tex.getU, tex.getV,     0, 0, -1,
    n, p, n,   col,    tex.getU2, tex.getV2,   0, 0, -1,
    // north face
    n, n, p,   col,    tex.getU, tex.getV,     0, 0, 1,
    p, n, p,   col,    tex.getU2, tex.getV,    0, 0, 1,
    p, p, p,   col,    tex.getU2, tex.getV2,   0, 0, 1,
    n, p, p,   col,    tex.getU,  tex.getV2,   0, 0, 1,
    // west face
    p, n, n,   col,    tex.getU2, tex.getV,    1, 0, 0,
    p, n, p,   col,    tex.getU, tex.getV,     1, 0, 0,
    p, p, p,   col,    tex.getU, tex.getV2,    1, 0, 0,
    p, p, n,   col,    tex.getU2, tex.getV2,   1, 0, 0,
    // east face
    n, n, n,   col,    tex.getU, tex.getV,     -1, 0, 0,
    n, n, p,   col,    tex.getU2, tex.getV,    -1, 0, 0,
    n, p, p,   col,    tex.getU2, tex.getV2,   -1, 0, 0,
    n, p, n,   col,    tex.getU, tex.getV2,    -1, 0, 0,
    // up face
    n, n, p,   col,    tex.getU2, tex.getV,    0, 0, 1,
    p, p, n,   col,    tex.getU, tex.getV,     0, 0, 1,
    p, p, p,   col,    tex.getU, tex.getV2,    0, 0, 1,
    n, p, p,   col,    tex.getU2, tex.getV2,   0, 0, 1,
    // down face
    n, n, n,   col,    tex.getU2, tex.getV2,   0, 0, -1,
    p, n, n,   col,    tex.getU, tex.getV2,    0, 0, -1,
    p, n, p,   col,    tex.getU, tex.getV,     0, 0, -1,
    n, n, p,   col,    tex.getU2, tex.getV,    0, 0, -1
  )
  val indices = Array[Short](
    // south face
    0, 1, 2,      0, 2, 3,
    // north face
    4, 6, 5,      4, 7, 6,
    // west face
    8, 9, 10,     8, 10, 11,
    // east face
    12, 14, 13,   12, 15, 14,
    // up face
    16, 17, 18,   16, 18, 19,
    // down face
    20, 22, 21,   20, 23, 22
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
