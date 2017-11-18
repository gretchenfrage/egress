package com.phoenixkahlo.hellcraft.core.client
import java.util.UUID

import com.badlogic.gdx.Gdx
import com.phoenixkahlo.hellcraft.core.{Blocks, SetMat, World}
import com.phoenixkahlo.hellcraft.core.client.ClientLogic.Input
import com.phoenixkahlo.hellcraft.math._
import com.badlogic.gdx.Input.Keys._
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.{BitmapFont, GlyphLayout, TextureRegion}
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.graphics.models.{BlockOutline, ChunkOutline}
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

case class GodClientMenu(pressed: Set[Int], buttons: Seq[MenuButton], pressing: Option[MenuButton], hud: HUD) extends ClientLogic {
  override def update(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    cause(ReleaseCursor)

  override def tick(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = {
    val chunkPos = input.camPos / 16 toInts
    val loadTarget = (chunkPos - GodClient.loadRad) to (chunkPos + GodClient.loadRad) toSet;
    cause(ReleaseCursor, SetLoadTarget(loadTarget))
  }

  override def keyDown(keycode: Int)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    if (keycode == ESCAPE) GodClient(pressed) -> Seq(CaptureCursor)
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
    val (buttons, hud) = GodClientMenu.makeButtonsAndHUD()
    become(copy(buttons = buttons, hud = hud))
  }
}

object GodClientMenu {
  def makeButtonsAndHUD(): (Seq[MenuButton], HUD) = {
    val center = V2I(Gdx.graphics.getWidth / 2, Gdx.graphics.getHeight / 2)
    val rad = V2I(150, 18)

    val buttons = new ArrayBuffer[MenuButton]
    buttons += MenuButton(center - rad + V2I(0, 50), center + rad + V2I(0, 50), "Resume Game", (c, w, i) => {
      println("resume game")
      GodClient(c.asInstanceOf[GodClientMenu].pressed) -> Seq(CaptureCursor)
    })
    buttons += MenuButton(center - rad - V2I(0, 50), center + rad - V2I(0, 50), "Exit Game", (c, w, i) => {
      println("exit game")
      c.cause(Exit)
    })

    val hud = MenuHUD(buttons)

    (buttons, hud)
  }

  def apply(pressed: Set[Int]): GodClientMenu = {
    val (buttons, hud) = makeButtonsAndHUD()
    GodClientMenu(pressed, buttons, None, hud)
  }
}

case class GodClient(pressed: Set[Int]) extends ClientLogic {
  override def update(world: World, input: Input) = {
    var movDir: V3F = Origin
    if (pressed(W)) movDir += input.camDir
    if (pressed(S)) movDir -= input.camDir
    if (pressed(D)) movDir += (input.camDir cross Up).normalize
    if (pressed(A)) movDir -= (input.camDir cross Up).normalize

    val chunkPos = input.camPos / 16 toInts
    val loadTarget = (chunkPos - GodClient.loadRad) to (chunkPos + GodClient.loadRad) toSet

    cause(
      SetCamPos(input.camPos + (movDir * GodClient.moveSpeed)),
      SetLoadTarget(loadTarget)
    )
  }

  override def keyDown(keycode: Int)(world: World, input: Input) =
    if (keycode == ESCAPE) become(GodClientMenu(pressed))
    else become(copy(pressed = pressed + keycode))

  override def keyUp(keycode: Int)(world: World, input: Input) =
    become(copy(pressed = pressed - keycode))

  override def touchDown(pos: V2I, pointer: Int, button: Button)(world: World, input: Input) =
    if (input.isCursorCaught) button match {
      case Right =>
        world
          .placeBlock(input.camPos, input.camDir, 64)
          .map(v => cause(CauseUpdateEffect(SetMat(v, Blocks.Brick, 16, UUID.randomUUID(), revalBlocks = true))))
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

  val hud = new GodHUD

  override def render(world: World, input: Input): (HUD, Seq[RenderUnit]) = {
    val units = new ArrayBuffer[RenderUnit]

    for (v <- world.placeBlock(input.camPos, input.camDir, 64)) {
      units += new BlockOutline(v, Color.WHITE, 0.95f)
    }

    if (Gdx.input.isKeyPressed(ALT_LEFT)) {
      val (complete, incomplete) = world.debugChunkMap.values.partition(_.isComplete)
      for (c <- complete) {
        units += new ChunkOutline(c.pos, Color.GREEN)
      }
      for (c <- incomplete) {
        units += new ChunkOutline(c.pos, Color.RED)
      }
    }

    hud -> units
  }

  override def resize(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = become(copy())
}

object Abe {
  val rant = "We can't bust heads like we used to, but we have our ways. One trick is to tell 'em stories that don't go anywhere - like the time I caught the ferry over to Shelbyville. I needed a new heel for my shoe, so, I decided to go to Morganville, which is what they called Shelbyville in those days. So I tied an onion to my belt, which was the style at the time."
}

class GodHUD extends HUD {
  val _components = new ParamCache[ResourcePack, Seq[HUDComponent]](pack => {
    val comps = new ArrayBuffer[HUDComponent]
    comps += ImgHUDComponent(
      pack(CrosshairTID),
      V2F(Gdx.graphics.getWidth / 2 - 15, Gdx.graphics.getHeight / 2 - 15),
      V2F(30, 30)
    )
    comps += ImgHUDComponent(
      new TextureRegion(pack.frame(MenuPatchPID, 18, Gdx.graphics.getWidth / 2, Gdx.graphics.getHeight / 2)),
      V2I(0, 0),
      V2I(Gdx.graphics.getWidth / 2, Gdx.graphics.getHeight / 2)
    )
    comps += StrBoxHUDComponent(
      Abe.rant,
      pack.font(ButtonFID),
      V2I(Gdx.graphics.getWidth / 2, Gdx.graphics.getHeight / 2), UpRight,
      V2I(Gdx.graphics.getWidth / 2, Gdx.graphics.getHeight / 2),
      Color.WHITE, 3, 10
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