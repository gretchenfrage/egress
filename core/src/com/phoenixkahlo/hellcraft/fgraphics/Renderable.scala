package com.phoenixkahlo.hellcraft.fgraphics

import java.util.Objects

import com.phoenixkahlo.hellcraft.ShaderTag
import com.phoenixkahlo.hellcraft.core.eval.GEval.GEval
import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.util.collections.ContextPin
import com.phoenixkahlo.hellcraft.util.collections.ContextPin.ContextPin

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Renderable[S <: Shader](eval: GEval[S#RenderUnit], transPos: Option[V3F], pin: ContextPin, alts: Vector[Renderable[S]])(implicit val shader: ShaderTag[S]) {
  def update(neu: GEval[S#RenderUnit], chainSize: Int = 10): Renderable[S] = {
    val appended = this.copy(alts = Vector.empty) +: alts
    Renderable(neu, transPos, ContextPin.create(), appended.dropRight(Math.max(0, appended.size - chainSize)))
  }

  def deupdate: Option[Renderable[S]] =
    if (alts.nonEmpty) Some(Renderable(alts.head.eval, transPos, alts.head.pin, alts.tail))
    else None
}
object Renderable {
  def apply[S <: Shader](eval: GEval[S#RenderUnit], transPos: Option[V3F] = None)(implicit shader: ShaderTag[S]): Renderable[S] =
    Renderable(eval, transPos, ContextPin.create(), Vector.empty)
}

case class Render[S <: Shader](renderable: Renderable[S], params: S#Params, mustRender: Boolean = false) {
  def deupdate: Option[Render[S]] =
    renderable.deupdate.map(r => copy(renderable = r))
}