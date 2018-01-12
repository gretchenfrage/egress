package com.phoenixkahlo.hellcraft.core.entity

import com.phoenixkahlo.hellcraft.core.{Event, PutEnt, UpdateEffect}
import com.phoenixkahlo.hellcraft.core.eval.{ExecSeq, GEval}
import com.phoenixkahlo.hellcraft.core.event.UE
import com.phoenixkahlo.hellcraft.core.graphics.{FreeCube, FreeCubeParams, RenderWorld}
import com.phoenixkahlo.hellcraft.fgraphics.LineShader.Vert
import com.phoenixkahlo.hellcraft.fgraphics.{GenericShader, LineShader, Offset, PhysTID, Render, Renderable}
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.math._

case class Avatar(
                   override val pos: V3F,
                   override val vel: V3F,
                   lastPos: V3F,
                   override val walkSpeed: Float,
                   override val walkDir: V2F,
                   jumpTimer: Int,
                   override val id: EntID[Avatar]
                 ) extends Walker[Avatar] {
  override val rad = 0.4f
  override val capHeight = 1.1f
  override val springRest = 0.8f
  override val springSnap = 1.6f
  override val springConstMovingDown = 5000
  override val springFriction = 300

  override val mass = 60

  override val walkForce = 5000

  override def springEnabled = jumpTimer == 0

  override def setPosVel(lastPos: V3F, newPos: V3F, newVel: V3F) =
    copy(pos = newPos, vel = newVel, lastPos = lastPos)

  def setStride(dir: V2F, speed: Float): Avatar = {
    copy(walkDir = dir, walkSpeed = speed)
  }

  def pos(interp: Float): V3F = pos + ((lastPos - pos) * interp)

  override def update =
    if (jumpTimer == 0) super.update
    else super.update :+ Event(UE.ent(id).map({
      case Some(ava) =>
        if (ava.jumpTimer == 0) Seq.empty
        else Seq(PutEnt(ava.copy(jumpTimer = ava.jumpTimer - 1)))
      case None => Seq.empty
    }))

  override def render(world: RenderWorld) =
    Seq.empty
    //Seq(Render[GenericShader](FreeCube(FreeCubeParams(PhysTID, V4I.ones)), Offset(pos(world.interp))))
    //Seq(Render[LineShader](Avatar.renderable, Offset(pos(world.interp))))
}

object Avatar {
  import scala.concurrent.duration._

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
    Avatar(pos, vel, pos, speed, walk, 0, EntID())

  def jump(id: EntID[Avatar], height: Float): Seq[UpdateEffect] = Seq(Event(UE.ent(id).map({
    case Some(ava) =>
      val jumpTimer = ((100 milliseconds) / Delta.dt).toInt
      val initVel = Math.sqrt(2 * 9.8f * height).toFloat
      Seq(PutEnt(ava.copy(jumpTimer = jumpTimer, vel = ava.vel.copy(y = initVel))))
    case None => Seq.empty
  })))
}
