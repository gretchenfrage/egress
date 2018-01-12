package com.phoenixkahlo.hellcraft.core.client

import java.util.concurrent.ThreadLocalRandom

import com.badlogic.gdx.Input.Keys._
import com.badlogic.gdx.graphics.Color
import com.phoenixkahlo.hellcraft.core.{Air, Blocks, Event, Materials, PutEnt}
import com.phoenixkahlo.hellcraft.core.client.ClientLogic.Input
import com.phoenixkahlo.hellcraft.core.client.ClientSessionData.{JumpHeight, LoadDist, Sensitivity}
import com.phoenixkahlo.hellcraft.core.entity.{Avatar, EntID}
import com.phoenixkahlo.hellcraft.core.event.{Events, UE}
import com.phoenixkahlo.hellcraft.fgraphics.{HUDShader, Render}
import com.phoenixkahlo.hellcraft.math._

trait ShouldCapture

case class AvatarClientMain(core: ClientCore, avaID: EntID[Avatar]) extends ClientLogic with ShouldCapture {
  val rands = ThreadLocal.withInitial[MRNG](() => new MRNG(ThreadLocalRandom.current.nextLong()))
  implicit def rand: MRNG = rands.get

  override def become(replacement: ClientLogic) =
    if (!replacement.isInstanceOf[ShouldCapture]) replacement -> Seq(ReleaseCursor)
    else replacement -> Seq.empty

  override def frame(world: ClientRenderWorld, input: ClientLogic.Input) = {
    world.findEntity(avaID) match {
      case Some(ava) =>
        ({
          var movDir: V3F = Origin
          if (core.pressed(W)) movDir += core.camDir
          if (core.pressed(S)) movDir -= core.camDir
          if (core.pressed(D)) movDir += (core.camDir cross Up).normalize
          if (core.pressed(A)) movDir -= (core.camDir cross Up).normalize

          val chunkPos = ava.chunkPos
          val loadTarget = (chunkPos - input.sessionData(LoadDist)).copy(y = -4).toInts toAsSet (chunkPos + input.sessionData(LoadDist)).copy(y = 4).toInts

          (
            copy(core = core.copy(camPos = ava.pos(world.interp))),
            Seq(
              SetLoadTarget(loadTarget, loadTarget.shroat(4)),
              CauseUpdateEffect(Event(UE.ent(avaID).map({
                case Some(ava) => Seq(PutEnt(ava.setStride(movDir.flatten.normalize, 6)))
                case None => Seq.empty
              })))
            )
          )
        }, {
          val (wrender, globals) = core.render(world, input)
          val hud = core.hud("", Color.CLEAR, input)
          (wrender ++ hud, globals)
        })
      case None => (nothing, core.render(world, input))
    }
  }

  override def keyDown(keycode: KeyCode)(world: ClientWorld, input: Input): (ClientLogic, Seq[ClientEffect]) = keycode match {
    case ESCAPE => become(AvatarClientMenu(core, input, avaID))
    case T => become(AvatarClientChat(core, avaID))
    case SLASH => become(AvatarClientChat(core, avaID, buffer = "/"))
    case SPACE => cause(CauseUpdateEffect(Avatar.jump(avaID, input.sessionData(JumpHeight))))
    case _ => become(copy(core = core.copy(pressed = core.pressed + keycode)))
  }

  override def keyUp(keycode: KeyCode)(world: ClientWorld, input: Input): (ClientLogic, Seq[ClientEffect]) =
    become(copy(core = core.copy(pressed = core.pressed - keycode)))

  override def touchDown(pos: V2I, pointer: Int, button: Button)(world: ClientWorld, input: Input): (ClientLogic, Seq[ClientEffect]) =
    if (input.isCursorCaught) button match {
      case Right =>
        world
          .placeBlock(core.camPos, core.camDir, 64)
          .map(v => cause(CauseUpdateEffect(Events.setMat(v, Blocks.Brick, mbf = true))))
          .getOrElse(nothing)
      case Left =>
        world
          .hit(core.camPos, core.camDir, 64)
          .map(v => cause(CauseUpdateEffect(Events.setMat(v, Air, mbf = true))))
          .getOrElse(nothing)
      case Middle =>
        world
          .placeMat(core.camPos, core.camDir, 64)
          .map(v => cause(CauseUpdateEffect(Events.setMat(v, Materials.Stone))))
          .getOrElse(nothing)
    } else cause(CaptureCursor)

  override def mouseMoved(pos: V2I, delta: V2I)(world: ClientWorld, input: Input): (ClientLogic, Seq[ClientEffect]) = {
    if (input.isCursorCaught) {
      var camDir = core.camDir

      val dx = -delta.x * input.sessionData(Sensitivity)
      camDir = camDir.rotate(Up, dx)

      var dy = -delta.y * input.sessionData(Sensitivity)
      val awd = core.camDir angleWith Down
      val awu = core.camDir angleWith Up
      if (awd + dy < GodClient.margin)
        dy = -awd + GodClient.margin
      else if (awu - dy < GodClient.margin)
        dy = awu - GodClient.margin
      camDir = camDir.rotate(camDir cross Up, dy)

      become(copy(core = core.copy(camDir = camDir)))
    } else nothing
  }


}

case class AvatarClientMenu(core: ClientCore, buttons: Seq[MenuButton], pressing: Option[MenuButton], avaID: EntID[Avatar]) extends ClientLogic {
  override def become(replacement: ClientLogic) =
    if (replacement.isInstanceOf[ShouldCapture]) replacement -> Seq(CaptureCursor)
    else replacement -> Seq.empty

  override def keyDown(keycode: KeyCode)(world: ClientWorld, input: Input): (ClientLogic, Seq[ClientEffect]) =
    if (keycode == ESCAPE) become(AvatarClientMain(core, avaID))
    else become(copy(core = core.copy(pressed = core.pressed + keycode)))

  override def touchDown(pos: V2I, pointer: KeyCode, button: Button)(world: ClientWorld, input: Input): (ClientLogic, Seq[ClientEffect]) =
    buttons.reverse.find(_ contains (pos, input)).map(button => become(copy(pressing = Some(button)))).getOrElse(nothing)

  override def touchUp(pos: V2I, pointer: KeyCode, button: Button)(world: ClientWorld, input: Input): (ClientLogic, Seq[ClientEffect]) =
    pressing match {
      case Some(pressing) => buttons.reverse.find(_ contains (pos, input)).filter(_ == pressing).map(_.onclick(this, world, input)).getOrElse(nothing)
      case None => nothing
    }

  override def frame(world: ClientRenderWorld, input: Input) = {
    val (renders, globals) = core.render(world, input)
    val menu: Seq[Render[HUDShader]] = buttons.flatMap(b => b.components match {
      case (off, on) => if (b contains (input.cursorPos, input)) on else off
    })
    (nothing, (renders ++: menu, globals))
  }
}
object AvatarClientMenu {
  def apply(core: ClientCore, input: Input, avaID: EntID[Avatar]): AvatarClientMenu = {
    val rad = V2I(150, 18)
    val buttons = Seq(
      MenuButton(
        res => {
          val center = res / 2 toInts;
          (center - rad + V2I(0, 50), center + rad + V2I(0, 50))
        },
        "Resume Game",
        (c, w, i) => {
          GodClientMain(c.asInstanceOf[GodClientMenu].core) -> Seq(
            CaptureCursor,
            ClientPrint("resuming game")
          )
        }),
      MenuButton(
        res => {
          val center = res / 2 toInts;
          (center - rad - V2I(0, 50), center + rad - V2I(0, 50))
        },
        "Exit Game",
        (c, w, i) => {
          c.cause(ClientPrint("exiting game"), Exit)
        })
    )
    AvatarClientMenu(core, buttons, None, avaID)
  }
}

case class AvatarClientChat(core: ClientCore, avaID: EntID[Avatar], cursor: Boolean = true, buffer: String = "") extends ClientLogic {
  override def become(replacement: ClientLogic) =
    if (replacement.isInstanceOf[ShouldCapture]) replacement -> Seq(CaptureCursor)
    else replacement -> Seq.empty

  def shouldCursor(input: Input): Boolean = {
    val interval = 1000000000
    input.nanoTime % interval > interval / 2
  }

  override def keyDown(keycode: KeyCode)(world: ClientWorld, input: Input): (ClientLogic, Seq[ClientEffect]) = keycode match {
    case ESCAPE => become(AvatarClientMain(core, avaID))
    case DEL => become(copy(buffer = buffer.dropRight(1)))
    case ENTER =>
      val (newChat, effects) = core.chat + (buffer, world, input)
      AvatarClientMain(core.copy(chat = newChat), avaID) -> (effects :+ CaptureCursor)
    case _ => input.keyToChar(keycode) match {
      case Some(c) => become(copy(buffer = buffer + c))
      case None => nothing
    }
  }

  override def frame(world: ClientRenderWorld, input: Input) = {
    (
      if (shouldCursor(input) ^ cursor)
        become(copy(cursor = shouldCursor(input)))
      else
        nothing,
      {
        val (wrender, globals) = core.render(world, input)
        val hud = core.hud(
          if (cursor) buffer + "|" else buffer,
          new Color(0.1718f, 0.2422f, 0.3125f, 0.3f),
          input
        )
        (wrender ++ hud, globals)
      }
    )
  }

}