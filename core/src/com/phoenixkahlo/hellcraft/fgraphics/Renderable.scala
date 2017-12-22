package com.phoenixkahlo.hellcraft.fgraphics

import java.util.Objects

import com.phoenixkahlo.hellcraft.ShaderTag
import com.phoenixkahlo.hellcraft.core.eval.GEval.GEval
import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.util.collections.ContextPin
import com.phoenixkahlo.hellcraft.util.collections.ContextPin.ContextPin

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/*
case class Renderable[S <: Shader](eval: GEval[S#RenderUnit], identityHash: Boolean = false,
                                   translucentPos: Option[V3F] = None, alts: Seq[Renderable[S]] = Seq.empty)
                                  (implicit val shader: ShaderTag[S], @transient var pin: Any = null) {
  /*
  def update(neu: GEval[S#RenderUnit], chainSize: Int = 10): Renderable[S] =
    copy(eval = neu, alts = (alts :+ eval).dropRight(Math.max(alts.size + 1 - chainSize, 0)))

  def deupdate: Option[Renderable[S]] =
    if (alts.nonEmpty) Some(copy(eval = alts.head, alts = alts.tail))
    else None
    */
  def update(neu: GEval[S#RenderUnit], chainSize: Int = 10): Renderable[S] = {
    val newAlts: Seq[Renderable[S]] = Renderable(eval, identityHash, translucentPos, Seq.empty)(shader, pin) +: alts
    Renderable(
      neu, identityHash, translucentPos,
      newAlts.dropRight(Math.max(newAlts.size - chainSize, 0))
    )
  }

  def deupdate: Option[Renderable[S]] =
    if (alts.nonEmpty) Some(Renderable(alts.head.eval, alts.head.identityHash, alts.head.translucentPos, alts.tail)(alts.head.shader, alts.head.pin))
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
  //@transient var pin: Any = null
}
*/
case class Renderable[S <: Shader](eval: GEval[S#RenderUnit], transPos: Option[V3F], pin: ContextPin, alts: Seq[Renderable[S]])(implicit val shader: ShaderTag[S]) {
  def update(neu: GEval[S#RenderUnit], chainSize: Int = 10): Renderable[S] = {
    val appended = this.copy(alts = Seq.empty) +: alts
    Renderable(neu, transPos, ContextPin.create(), appended.dropRight(Math.max(0, appended.size - chainSize)))
  }

  def deupdate: Option[Renderable[S]] =
    if (alts.nonEmpty) Some(Renderable(alts.head.eval, transPos, alts.head.pin, alts.tail))//Some(copy(eval = alts.head.eval, alts = alts.tail))
    else None
}
object Renderable {
  def apply[S <: Shader](eval: GEval[S#RenderUnit], transPos: Option[V3F] = None)(implicit shader: ShaderTag[S]): Renderable[S] =
    Renderable(eval, transPos, ContextPin.create(), Seq.empty)
}

case class Render[S <: Shader](renderable: Renderable[S], params: S#Params, mustRender: Boolean = false) {
  def deupdate: Option[Render[S]] =
    renderable.deupdate.map(r => copy(renderable = r))
}