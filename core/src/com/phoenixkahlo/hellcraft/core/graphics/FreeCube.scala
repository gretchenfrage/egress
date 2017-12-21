package com.phoenixkahlo.hellcraft.core.graphics

import com.phoenixkahlo.hellcraft.core.eval.{ExecSeq, GEval}
import com.phoenixkahlo.hellcraft.fgraphics._
import com.phoenixkahlo.hellcraft.math.{V2F, V3F, V4F}
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc

case class FreeCubeParams(tex: SheetTextureID, col: V4F)
object FreeCube extends MemoFunc[FreeCubeParams, Renderable[GenericShader]](params => {
  implicit val exec = ExecSeq
  Renderable[GenericShader](GEval.resourcePack.map(pack => {
    val n = -0.5f
    val p = +0.5f
    val tex = pack(params.tex)
    val verts = Seq[BasicTriVert](
      // south face
      BasicTriVert(V3F(n, n, n),   params.col,    V2F(tex.getU2, tex.getV),    V3F(0, 0, -1)),  // 0
      BasicTriVert(V3F(p, n, n),   params.col,    V2F(tex.getU, tex.getV),     V3F(0, 0, -1)),  // 1
      BasicTriVert(V3F(p, p, n),   params.col,    V2F(tex.getU, tex.getV2),    V3F(0, 0, -1)),  // 2
      BasicTriVert(V3F(n, p, n),   params.col,    V2F(tex.getU2, tex.getV2),   V3F(0, 0, -1)),  // 3
      // north face
      BasicTriVert(V3F(n, n, p),   params.col,    V2F(tex.getU, tex.getV),     V3F(0, 0, 1)),   // 4
      BasicTriVert(V3F(p, n, p),   params.col,    V2F(tex.getU2, tex.getV),    V3F(0, 0, 1)),   // 5
      BasicTriVert(V3F(p, p, p),   params.col,    V2F(tex.getU2, tex.getV2),   V3F(0, 0, 1)),   // 6
      BasicTriVert(V3F(n, p, p),   params.col,    V2F(tex.getU,  tex.getV2),   V3F(0, 0, 1)),   // 7
      // west face
      BasicTriVert(V3F(p, n, n),   params.col,    V2F(tex.getU2, tex.getV),    V3F(1, 0, 0)),   // 8
      BasicTriVert(V3F(p, n, p),   params.col,    V2F(tex.getU, tex.getV),     V3F(1, 0, 0)),   // 9
      BasicTriVert(V3F(p, p, p),   params.col,    V2F(tex.getU, tex.getV2),    V3F(1, 0, 0)),   // 10
      BasicTriVert(V3F(p, p, n),   params.col,    V2F(tex.getU2, tex.getV2),   V3F(1, 0, 0)),   // 11
      // east face
      BasicTriVert(V3F(n, n, n),   params.col,    V2F(tex.getU, tex.getV),     V3F(-1, 0, 0)),  // 12
      BasicTriVert(V3F(n, n, p),   params.col,    V2F(tex.getU2, tex.getV),    V3F(-1, 0, 0)),  // 13
      BasicTriVert(V3F(n, p, p),   params.col,    V2F(tex.getU2, tex.getV2),   V3F(-1, 0, 0)),  // 14
      BasicTriVert(V3F(n, p, n),   params.col,    V2F(tex.getU, tex.getV2),    V3F(-1, 0, 0)),  // 15
      // up face
      BasicTriVert(V3F(n, p, n),   params.col,    V2F(tex.getU2, tex.getV),    V3F(0, 0, 1)),   // 16
      BasicTriVert(V3F(p, p, n),   params.col,    V2F(tex.getU, tex.getV),     V3F(0, 0, 1)),   // 17
      BasicTriVert(V3F(p, p, p),   params.col,    V2F(tex.getU, tex.getV2),    V3F(0, 0, 1)),   // 18
      BasicTriVert(V3F(n, p, p),   params.col,    V2F(tex.getU2, tex.getV2),   V3F(0, 0, 1)),   // 19
      // down face
      BasicTriVert(V3F(n, n, n),   params.col,    V2F(tex.getU2, tex.getV2),   V3F(0, 0, -1)),  // 20
      BasicTriVert(V3F(p, n, n),   params.col,    V2F(tex.getU, tex.getV2),    V3F(0, 0, -1)),  // 21
      BasicTriVert(V3F(p, n, p),   params.col,    V2F(tex.getU, tex.getV),     V3F(0, 0, -1)),  // 22
      BasicTriVert(V3F(n, n, p),   params.col,    V2F(tex.getU2, tex.getV),    V3F(0, 0, -1))   // 23
    )
    val indices = Seq[Short](
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
    verts -> indices
  }))
})