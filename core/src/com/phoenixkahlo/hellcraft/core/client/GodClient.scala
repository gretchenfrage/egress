package com.phoenixkahlo.hellcraft.core.client
import java.util.UUID

import com.badlogic.gdx.Gdx
import com.phoenixkahlo.hellcraft.core.{Blocks, SetMat, World}
//import com.phoenixkahlo.hellcraft.core.{Blocks, SetMat, World}
import com.phoenixkahlo.hellcraft.core.client.ClientLogic.Input
import com.phoenixkahlo.hellcraft.math._
import com.badlogic.gdx.Input.Keys._
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.{BitmapFont, GlyphLayout, TextureRegion}
import com.phoenixkahlo.hellcraft.core.entity.CubeRenderer
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.graphics.models.{BlockOutline, ChunkOutline}
import com.phoenixkahlo.hellcraft.math
import com.phoenixkahlo.hellcraft.util.caches.ParamCache
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc

import scala.collection.mutable.ArrayBuffer

case class MenuButton(min: V2I, max: V2I, str: String, onclick: (ClientLogic, World, ClientLogic.Input) => ClientLogic.Output) {
  def contains(v: V2I): Boolean = {
    val v2 = V2I(v.xi, Gdx.graphics.getHeight - v.yi)
    v2 >= min && v2 <= max
  }
}

case class MenuHUD(buttons: Seq[MenuButton]) extends HUD {
  val _components = new ParamCache[ResourcePack, Seq[(MenuButton, Seq[HUDComponent], Seq[HUDComponent])]](pack => {
    val font = pack.font(ButtonFID)
    buttons.map(button => {
      val layout = new GlyphLayout
      layout.setText(font, button.str)

      val frameOff = ImgHUDComponent(
        new TextureRegion(pack.frame(MenuPatchPID, 18, button.max.xi - button.min.xi, button.max.yi - button.min.yi)),
        button.min,
        button.max - button.min
      )
      val frameOn = ImgHUDComponent(
        new TextureRegion(pack.frame(MenuPatchActivePID, 18, button.max.xi - button.min.xi, button.max.yi - button.min.yi)),
        button.min,
        button.max - button.min
      )
      val text = StrHUDComponent(
        button.str, font,
        (button.min + button.max) / 2 + V2F(-layout.width / 2, layout.height / 2),
        Color.BLACK
      )

      (
        button,
        Seq(frameOff, text),
        Seq(frameOn, text)
      )
    })
  })

  override def components(texturePack: ResourcePack): Seq[HUDComponent] = {
    _components(texturePack)
      .flatMap({ case (button, off, on) => if (button.contains(V2I(Gdx.input.getX, Gdx.input.getY))) on else off })
  }
}

case class GodClientMenu(pressed: Set[Int], buttons: Seq[MenuButton], pressing: Option[MenuButton], hud: HUD, chat: Chat) extends ClientLogic {

  override def become(replacement: ClientLogic): (ClientLogic, Seq[ClientEffect]) = {
    if (replacement.isInstanceOf[GodClient]) replacement -> Seq(CaptureCursor)
    else super.become(replacement)
  }

  override def tick(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = {
    val chunkPos = input.camPos / 16 toInts
    val loadTarget = (chunkPos - GodClient.loadRad) toAsSet (chunkPos + GodClient.loadRad) toSet;
    cause(SetLoadTarget(loadTarget))
  }

  override def keyDown(keycode: Int)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    if (keycode == ESCAPE) GodClient(pressed, chat) -> Seq(CaptureCursor)
    else become(copy(pressed = pressed + keycode))

  override def keyUp(keycode: Int)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    become(copy(pressed = pressed - keycode))

  override def touchDown(pos: V2I, pointer: Int, button: Button)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    buttons.reverse.find(_ contains pos).map(button => become(copy(pressing = Some(button)))).getOrElse(nothing)

  override def touchUp(pos: V2I, pointer: Int, button: Button)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    if (pressing isDefined) buttons.reverse.find(_ contains pos).filter(_ == pressing.get).map(_.onclick(this, world, input)).getOrElse(nothing)
    else nothing

  override def render(world: World, input: Input): (HUD, Seq[RenderUnit]) = {
    hud -> Seq.empty
  }

  override def resize(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = {
    val (buttons, hud) = GodClientMenu.makeButtonsAndHUD(chat)
    become(copy(buttons = buttons, hud = hud))
  }
}

object GodClientMenu {
  def makeButtonsAndHUD(chat: Chat): (Seq[MenuButton], HUD) = {
    val center = V2I(Gdx.graphics.getWidth / 2, Gdx.graphics.getHeight / 2)
    val rad = V2I(150, 18)

    val buttons = new ArrayBuffer[MenuButton]
    buttons += MenuButton(center - rad + V2I(0, 50), center + rad + V2I(0, 50), "Resume Game", (c, w, i) => {
      println("resume game")
      GodClient(c.asInstanceOf[GodClientMenu].pressed, chat) -> Seq(CaptureCursor)
    })
    buttons += MenuButton(center - rad - V2I(0, 50), center + rad - V2I(0, 50), "Exit Game", (c, w, i) => {
      println("exit game")
      c.cause(Exit)
    })

    val hud = MenuHUD(buttons)

    (buttons, hud)
  }

  def apply(pressed: Set[Int], chat: Chat): GodClientMenu = {
    val (buttons, hud) = makeButtonsAndHUD(chat)
    GodClientMenu(pressed, buttons, None, hud, chat)
  }
}

case class GodClientChat(chat: Chat, cursorOn: Boolean = true, buffer: String = "") extends ClientLogic {
  val hud = new GodHUD(
    chat,
    buffer + (if (cursorOn || buffer.isEmpty) "_" else ""),
    new Color(0.1718f, 0.2422f, 0.3125f, 0.3f)
  )

  override def become(replacement: ClientLogic): (ClientLogic, Seq[ClientEffect]) = {
    if (replacement.isInstanceOf[GodClient]) replacement -> Seq(CaptureCursor)
    else super.become(replacement)
  }

  override def update(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    if (cursorShouldBeOn(input) ^ cursorOn) become(copy(cursorOn = cursorShouldBeOn(input)))
    else nothing


  override def resize(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    become(copy())

  def cursorShouldBeOn(input: Input): Boolean = {
    val interval = 1000000000
    input.nanoTime % interval > interval / 2
  }

  override def keyDown(keycode: Int)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = keycode match {
    case ESCAPE => become(GodClient(Set.empty, chat))
    case DEL => become(copy(buffer = buffer.dropRight(1)))
    case ENTER =>
      val (newChat, effects) = chat + (buffer, world, input)
      GodClient(Set.empty, newChat) -> (effects :+ CaptureCursor)
    case k => input.keyToChar(k) match {
      case Some(c) => become(copy(buffer = buffer + c))
      case None => nothing
    }
  }

  override def render(world: World, input: Input): (HUD, Seq[RenderUnit]) = (hud, Seq.empty)
}

case class GodClient(pressed: Set[Int], chat: Chat) extends ClientLogic {

  override def become(replacement: ClientLogic): (ClientLogic, Seq[ClientEffect]) = {
    if (!replacement.isInstanceOf[GodClient]) replacement -> Seq(ReleaseCursor)
    else super.become(replacement)
  }

  override def update(world: World, input: Input) = {
    var movDir: V3F = Origin
    if (pressed(W)) movDir += input.camDir
    if (pressed(S)) movDir -= input.camDir
    if (pressed(D)) movDir += (input.camDir cross Up).normalize
    if (pressed(A)) movDir -= (input.camDir cross Up).normalize

    val chunkPos = input.camPos / 16 toInts
    val loadTarget = (chunkPos - GodClient.loadRad) toAsSet (chunkPos + GodClient.loadRad) toSet

    cause(
      SetCamPos(input.camPos + (movDir * GodClient.moveSpeed)),
      SetLoadTarget(loadTarget)
    )
  }

  override def keyDown(keycode: Int)(world: World, input: Input) = keycode match {
    case ESCAPE => become(GodClientMenu(pressed, chat))
    case T => become(GodClientChat(chat))
    case SLASH => become(GodClientChat(chat, buffer = "/"))
    case _ => become(copy(pressed = pressed + keycode))
  }

  override def keyUp(keycode: Int)(world: World, input: Input) =
    become(copy(pressed = pressed - keycode))

  override def touchDown(pos: V2I, pointer: Int, button: Button)(world: World, input: Input) =
    if (input.isCursorCaught) button match {
      case Right =>
        world
          .placeBlock(input.camPos, input.camDir, 64)
          .map(v => cause(CauseUpdateEffect(SetMat(v, Blocks.Brick, UUID.randomUUID(), meshBlocksFast = true))))
          .getOrElse(nothing)
      case _ => nothing
    } else cause(CaptureCursor)


  override def touchDragged(pos: V2I, delta: V2I, pointer: Int)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    mouseMoved(pos, delta)(world, input)

  override def mouseMoved(pos: V2I, delta: V2I)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    if (input.isCursorCaught) {
      var camDir = input.camDir

      val dx = -delta.x * GodClient.turnSpeed
      camDir = camDir.rotate(Up, dx)

      var dy = -delta.y * GodClient.turnSpeed
      val awd = input.camDir angleWith Down
      val awu = input.camDir angleWith Up
      if (awd + dy < GodClient.margin)
        dy = -awd + GodClient.margin
      else if (awu - dy < GodClient.margin)
        dy = awu - GodClient.margin
      camDir = camDir.rotate(camDir cross Up, dy)

      cause(SetCamDir(camDir))
    } else nothing

  val hud = new GodHUD(chat, "", Color.CLEAR)

  override def render(world: World, input: Input): (HUD, Seq[RenderUnit]) = {
    val units = new ArrayBuffer[RenderUnit]

    for (v <- world.placeBlock(input.camPos, input.camDir, 64)) {
      units += new BlockOutline(v, Color.WHITE, 0.95f)
    }

    if (pressed(ALT_LEFT)) {
      for (c <- world.debugChunkMap.keys) {
        units += new ChunkOutline(c, Color.WHITE)
      }
    }
    if (input.sessionData.get("show_tasks").exists(_.asInstanceOf[Boolean])) {
      val (tasks3D, tasks2D, tasksDB3D) = input.executor.getSpatialTasks
      for (p <- tasks3D) {
        units += CubeRenderer(GrayTID, Color.WHITE, p)(input.pack)
      }
      for (p <- tasks2D.map(_.inflate(0))) {
        units += CubeRenderer(GrayTID, Color.BLUE, p)(input.pack)
      }
      for (p <- tasksDB3D) {
        units += CubeRenderer(GrayTID, Color.GREEN, p)(input.pack)
      }
    }

    hud -> units
  }

  override def resize(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = become(copy())
}

class GodHUD(chat: Chat, buffer: String = "", bg: Color) extends HUD {
  val _components = new ParamCache[ResourcePack, Seq[HUDComponent]](pack => {
    val comps = new ArrayBuffer[HUDComponent]
    comps += ImgHUDComponent(
      pack(CrosshairTID),
      V2F(Gdx.graphics.getWidth / 2 - 15, Gdx.graphics.getHeight / 2 - 15),
      V2F(30, 30)
    )
    comps += ImgHUDComponent(
      new TextureRegion(pack.dot(bg)),
      Origin2D,
      V2F(Gdx.graphics.getWidth * 0.4f, Gdx.graphics.getHeight / 2)
    )
    val str = (chat.messages :+ buffer).foldLeft("")(_ + _ + '\n').dropRight(1)
    comps += StrBoxHUDComponent(
      str, pack.font(ChatFID),
      Origin2D, DownLeft, V2F(Gdx.graphics.getWidth * 0.4f, Gdx.graphics.getHeight / 2),
      new Color(1f, 1f, 1f, 0.5f), 3, 10
    )
    comps
  })

  override def components(texturePack: ResourcePack): Seq[HUDComponent] = {
    _components(texturePack)
  }
}

object GodClient {
  val turnSpeed = 0.25f
  val moveSpeed = 1f
  val margin = 1f
  val loadRad = V3I(12, 5, 12)
}