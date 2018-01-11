package com.phoenixkahlo.hellcraft.core.graphics

import com.phoenixkahlo.hellcraft.core.eval.{ExecHint, GEval, WEval}
import com.phoenixkahlo.hellcraft.fgraphics.{LineShader, Renderable}

import scala.collection.mutable

object LineCombiner {
  // warning: is synchronous
  def apply(units: => Seq[LineShader#RenderUnit])(implicit exec: ExecHint): Seq[LineShader#RenderUnit] = {
    val max = Short.MaxValue
    def f(list: List[LineShader#RenderUnit], vaccum: Vector[LineShader.Vert], iaccum: Vector[Short]): List[LineShader#RenderUnit] =
      list match {
        case Nil => List((vaccum, iaccum))
        case (currVerts, currIndices) :: next =>
          if (vaccum.size + currVerts.size >= max || iaccum.size + currIndices.size >= max)
            (vaccum, iaccum) :: f(list, Vector.empty, Vector.empty)
          else
            f(next, vaccum ++ currVerts, iaccum ++ currIndices.map(i => (i + vaccum.size).toShort))
      }
    f(units.toList, Vector.empty, Vector.empty).toVector
  }

}
