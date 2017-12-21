package com.phoenixkahlo.hellcraft.fgraphics

import java.util.Objects

import com.phoenixkahlo.hellcraft.core.eval.GEval.GEval
import com.phoenixkahlo.hellcraft.math.V3F


case class Renderable[S <: Shader](eval: GEval[S#RenderUnit], identityHash: Boolean = false, translucentPos: Option[V3F] = None)(implicit val shader: ShaderTag[S]) {
  override def hashCode(): Int =
    if (identityHash) super.hashCode()
    else Objects.hash(eval, identityHash.asInstanceOf[AnyRef], translucentPos)

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case ref: AnyRef if identityHash => this eq ref
      case Renderable(e, i, t) => eval == e && identityHash == i && translucentPos == t
      case _ => false
    }

  @transient var pin: Any = null
}

case class Render[S <: Shader](renderable: Renderable[S], params: S#Params, mustRender: Boolean = false)