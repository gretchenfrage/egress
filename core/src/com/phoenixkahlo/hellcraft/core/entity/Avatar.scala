package com.phoenixkahlo.hellcraft.core.entity

import com.phoenixkahlo.hellcraft.core.eval.{ExecSeq, GEval}
import com.phoenixkahlo.hellcraft.core.graphics.RenderWorld
import com.phoenixkahlo.hellcraft.fgraphics.LineShader.Vert
import com.phoenixkahlo.hellcraft.fgraphics.{GenericShader, LineShader, Offset, Render, Renderable}
import com.phoenixkahlo.hellcraft.math.{MRNG, Origin, V3F, V4I}

case class Avatar(override val pos: V3F, override val vel: V3F, lastPos: V3F, override val id: EntID[Avatar]) extends Walker[Avatar] {
  override val rad = 0.4f
  override val capHeight = 1.1f
  override val springRest = 0.8f
  override val springSnap = 1.05f
  override val springConst = 6

  val height = capHeight + springRest

  override def setPosVel(lastPos: V3F, newPos: V3F, newVel: V3F) =
    copy(pos = newPos, vel = newVel, lastPos = lastPos)

  def pos(interp: Float): V3F = pos + ((lastPos - pos) * interp)

  override def render(world: RenderWorld) =
    Seq(Render[LineShader](Avatar.renderable, Offset(pos(world.interp))))
}

object Avatar {
  val renderable: Renderable[LineShader] = {
    implicit val exec = ExecSeq
    Renderable[LineShader](GEval(
      (
        Seq(Vert(V3F(0, -1.35f, 0), V4I(1, 0, 0, 1)), Vert(V3F(0, 0.55f, 0), V4I(0, 1, 0, 1))),
        Seq(0, 1).map(_.toShort)
      )
    ))
  }

  def apply(pos: V3F, vel: V3F = Origin)(implicit rand: MRNG): Avatar =
    Avatar(pos, vel, pos, EntID())
}
