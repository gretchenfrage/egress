package com.phoenixkahlo.hellcraft.core.entity

import com.phoenixkahlo.hellcraft.core.eval.{ExecSeq, GEval}
import com.phoenixkahlo.hellcraft.core.graphics.{FreeCube, FreeCubeParams, RenderWorld}
import com.phoenixkahlo.hellcraft.fgraphics.LineShader.Vert
import com.phoenixkahlo.hellcraft.fgraphics.{GenericShader, LineShader, Offset, PhysTID, Render, Renderable}
import com.phoenixkahlo.hellcraft.math._

case class Avatar(
                   override val pos: V3F,
                   override val vel: V3F,
                   lastPos: V3F,
                   override val walkSpeed: Float,
                   override val walkDir: V2F,
                   override val id: EntID[Avatar]
                 ) extends Walker[Avatar] {
  override val rad = 0.4f
  override val capHeight = 1.1f
  override val springRest = 0.8f
  override val springSnap = 1.6f
  override val springConstMovingDown = 1000
  override val springConstMovingUp = springConstMovingDown
  override val springFriction = 500

  override val mass = 60

  override val walkForce = 5000

  val height = capHeight + springRest

  override def setPosVel(lastPos: V3F, newPos: V3F, newVel: V3F) =
    copy(pos = newPos, vel = newVel, lastPos = lastPos)

  def setStride(dir: V2F, speed: Float): Avatar = {
    copy(walkDir = dir, walkSpeed = speed)
  }

  def pos(interp: Float): V3F = pos + ((lastPos - pos) * interp)

  override def render(world: RenderWorld) =
    Seq(Render[GenericShader](FreeCube(FreeCubeParams(PhysTID, V4I.ones)), Offset(pos(world.interp))))
    //Seq(Render[LineShader](Avatar.renderable, Offset(pos(world.interp))))
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

  def apply(pos: V3F, vel: V3F = Origin, walk: V2F = Origin2D, speed: Float = 0)(implicit rand: MRNG): Avatar =
    Avatar(pos, vel, pos, speed, walk, EntID())
}
