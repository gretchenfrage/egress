package com.phoenixkahlo.hellcraft.core.graphics

import com.phoenixkahlo.hellcraft.core.eval.{ExecHint, GEval, WEval}
import com.phoenixkahlo.hellcraft.fgraphics.{LineShader, Renderable}

import scala.collection.mutable

object LineCombiner {
  // warning: is synchronous
  def apply(units: => Seq[LineShader#RenderUnit])(implicit exec: ExecHint): Seq[Renderable[LineShader]] = {
    val renderables = new mutable.ArrayBuffer[Renderable[LineShader]]
    var verts = new mutable.ArrayBuffer[LineShader.Vert]
    var indices = new mutable.ArrayBuffer[Short]
    def push() = {
      renderables += Renderable[LineShader](GEval((verts, indices)))
      verts = new mutable.ArrayBuffer[LineShader.Vert]
      indices = new mutable.ArrayBuffer[Short]
    }
    for ((v, i) <- units) {
      val offset = verts.size
      if ((offset + i.size) >= Short.MaxValue)
        push()
      verts ++= v
      indices ++= i.map(ii => (ii + offset).toShort)
    }
    push()
    renderables
  }

}
