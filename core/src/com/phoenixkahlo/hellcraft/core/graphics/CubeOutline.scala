package com.phoenixkahlo.hellcraft.core.graphics

import java.awt.Color

import com.phoenixkahlo.hellcraft.core.eval.ExecSeq
import com.phoenixkahlo.hellcraft.fgraphics._
import com.phoenixkahlo.hellcraft.math.{V3F, V4F}
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc

case class CubeOutlineParams(color: V4F, size: Float)
object CubeOutline extends MemoFunc[CubeOutlineParams, Renderable[LineShader]](params => {
  import LineShader.Vert
  implicit val exec = ExecSeq
  Renderable[LineShader](GEval({
    val n = params.size * -0.5f
    val p = params.size * +0.5f
    val verts = Seq[Vert](
      Vert(V3F(n, n, n), params.color),
      Vert(V3F(p, n, n), params.color),
      Vert(V3F(p, n, p), params.color),
      Vert(V3F(n, n, p), params.color),
      Vert(V3F(n, p, n), params.color),
      Vert(V3F(p, p, n), params.color),
      Vert(V3F(p, p, p), params.color),
      Vert(V3F(n, p, p), params.color)
    )
    val indices = Seq[Short](
      0, 1,
      1, 2,
      2, 3,
      3, 0,
      4, 5,
      5, 6,
      6, 7,
      7, 4,
      0, 4,
      1, 5,
      2, 6,
      3, 7
    )
    verts -> indices
  }))
}) {
  def block(color: V4F) = this(CubeOutlineParams(color, 1 - 2e-3f))
  def chunk(color: V4F) = this(CubeOutlineParams(color, 15))
}