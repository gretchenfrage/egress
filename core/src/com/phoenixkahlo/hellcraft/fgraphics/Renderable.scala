package com.phoenixkahlo.hellcraft.fgraphics

import java.util.Objects

import com.phoenixkahlo.hellcraft.ShaderTag
import com.phoenixkahlo.hellcraft.core.eval.GEval.GEval
import com.phoenixkahlo.hellcraft.math.V3F

import scala.annotation.tailrec
import scala.collection.immutable.Queue


case class Renderable[S <: Shader](eval: GEval[S#RenderUnit], identityHash: Boolean = false,
                                   translucentPos: Option[V3F] = None, alts: Seq[GEval[S#RenderUnit]] = Queue.empty)
                                  (implicit val shader: ShaderTag[S]) {
  def update(neu: GEval[S#RenderUnit], chainSize: Int = 10): Renderable[S] =
    copy(eval = neu, alts = (alts :+ eval).dropRight(Math.max(alts.size + 1 - chainSize, 0)))

  def deupdate: Option[Renderable[S]] =
    if (alts.nonEmpty) Some(copy(eval = alts.head, alts = alts.tail))
    else None

  override def hashCode(): Int =
    if (identityHash) super.hashCode()
    else Objects.hash(eval, translucentPos)

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case ref: AnyRef if identityHash => this eq ref
      case Renderable(e, _, t, _) => eval == e && translucentPos == t
      case _ => false
    }

  @transient var pin: Any = null
}

case class Render[S <: Shader](renderable: Renderable[S], params: S#Params, mustRender: Boolean = false) {
  def deupdate: Option[Render[S]] =
    renderable.deupdate.map(r => copy(renderable = r))
}