package com.phoenixkahlo.hellcraft.core.client
import java.util.UUID

import com.badlogic.gdx.Gdx
import com.phoenixkahlo.hellcraft.core.request.ExecCheap
import com.phoenixkahlo.hellcraft.core.{Blocks, RenderWorld, SetMat, World}
import com.phoenixkahlo.hellcraft.fgraphics._
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
  
  val components: (Seq[Render[HUDShader]], Seq[Render[HUDShader]]) = {
    implicit val exec = ExecCheap
    val frameOff = GEval.resourcePack.map(pack => ImgHUDComponent(
      new TextureRegion(pack.frame(MenuPatchPID, 18, max.xi - min.xi, max.yi - min.yi)),
      min,
      max - min
    ))
    val frameOn = GEval.resourcePack.map(pack => ImgHUDComponent(
      new TextureRegion(pack.frame(MenuPatchActivePID, 18, max.xi - min.xi, max.yi - min.yi)),
      min,
      max - min
    ))
    val text = GEval.resourcePack.map(pack => {
      val font = pack.font(ButtonFID)
      val layout = new GlyphLayout
      layout.setText(font, str)
      StrHUDComponent(
        str, font,
        (min + max) / 2 + V2F(-layout.width / 2, layout.height / 2),
        Color.BLACK
      )
    })
    (
      Seq(frameOff, text).map(Renderable[HUDShader](_)).map(Render[HUDShader](_, (): Unit)),
      Seq(frameOn, text).map(Renderable[HUDShader](_)).map(Render[HUDShader](_, (): Unit))
    )
  }
}

case class ClientCore(pressed: Set[KeyCode], chat: Chat, camPos: V3F, camDir: V3F) {
  def render(world: RenderWorld, input: Input): (Seq[Render[_ <: Shader]], GlobalRenderData) = {
    val renders = new ArrayBuffer[Render[_ <: Shader]]

    for (render <- world.renderableChunks.flatMap(_.renders)) {
      renders += render
    }

    val globals = GlobalRenderData(camPos, camDir, Origin, 1, V4F(0, 0, 1, 1), 90)

    renders -> globals
  }
}

case class GodClientMain(core: ClientCore) extends ClientLogic {
  override def become(replacement: ClientLogic): (ClientLogic, Seq[ClientEffect]) = {
    if (!replacement.isInstanceOf[GodClientMain]) replacement -> Seq(ReleaseCursor)
    else super.become(replacement)
  }

  override def update(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = {
    var movDir: V3F = Origin
    if (core.pressed(W)) movDir += core.camDir
    if (core.pressed(S)) movDir -= core.camDir
    if (core.pressed(D)) movDir += (core.camDir cross Up).normalize
    if (core.pressed(A)) movDir -= (core.camDir cross Up).normalize

    val chunkPos = core.camPos / 16 toInts
    val loadTarget = (chunkPos - GodClient.loadRad) toAsSet (chunkPos + GodClient.loadRad)

    GodClientMain(core.copy(camPos = core.camPos + (movDir * GodClient.moveSpeed))) -> Seq(
      SetLoadTarget(loadTarget, loadTarget.shroat(4))
    )
  }

  override def keyDown(keycode: KeyCode)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = keycode match {
    case ESCAPE => become(GodClientMenu(core, input))
    case _ => become(GodClientMain(core.copy(pressed = core.pressed + keycode)))
  }

  override def keyUp(keycode: KeyCode)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    become(GodClientMain(core.copy(pressed = core.pressed - keycode)))

  override def touchDown(pos: V2I, pointer: Int, button: Button)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    if (input.isCursorCaught) button match {
      case Right =>
        world
          .placeBlock(core.camPos, core.camDir, 64)
          .map(v => cause(CauseUpdateEffect(SetMat(v, Blocks.Brick, UUID.randomUUID(), meshBlocksFast = true))))
          .getOrElse(nothing)
      case _ => nothing
    } else cause(CaptureCursor)

  override def mouseMoved(pos: V2I, delta: V2I)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = {
    if (input.isCursorCaught) {
      var camDir = core.camDir

      val dx = -delta.x * GodClient.turnSpeed
      camDir = camDir.rotate(Up, dx)

      var dy = -delta.y * GodClient.turnSpeed
      val awd = core.camDir angleWith Down
      val awu = core.camDir angleWith Up
      if (awd + dy < GodClient.margin)
        dy = -awd + GodClient.margin
      else if (awu - dy < GodClient.margin)
        dy = awu - GodClient.margin
      camDir = camDir.rotate(camDir cross Up, dy)

      become(GodClientMain(core.copy(camDir = camDir)))
    } else nothing
  }

  override def render(world: RenderWorld, input: Input): (Seq[Render[_ <: Shader]], GlobalRenderData) =
    core.render(world, input)
}

case class GodClientMenu(core: ClientCore, buttons: Seq[MenuButton], pressing: Option[MenuButton]) extends ClientLogic {
  override def become(replacement: ClientLogic): (ClientLogic, Seq[ClientEffect]) = {
    if (replacement.isInstanceOf[GodClientMain]) replacement -> Seq(CaptureCursor)
    else super.become(replacement)
  }

  override def tick(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = {
    val chunkPos = core.camPos / 16 toInts
    val loadTarget = (chunkPos - GodClient.loadRad) toAsSet (chunkPos + GodClient.loadRad)
    cause(SetLoadTarget(loadTarget, loadTarget.shroat(4)))
  }

  override def keyDown(keycode: KeyCode)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    if (keycode == ESCAPE) become(GodClientMain(core))
    else become(copy(core = core.copy(pressed = core.pressed + keycode)))


  override def touchDown(pos: V2I, pointer: KeyCode, button: Button)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    buttons.reverse.find(_ contains pos).map(button => become(copy(pressing = Some(button)))).getOrElse(nothing)

  override def touchUp(pos: V2I, pointer: KeyCode, button: Button)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    pressing match {
      case Some(pressing) => buttons.reverse.find(_ contains pos).filter(_ == pressing).map(_.onclick(this, world, input)).getOrElse(nothing)
      case None => nothing
    }

  override def render(world: RenderWorld, input: Input): (Seq[Render[_ <: Shader]], GlobalRenderData) = {
    val (renders, globals) = core.render(world, input)
    val menu: Seq[Render[HUDShader]] = buttons.flatMap(b => b.components match {
      case (off, on) => if (b contains input.cursorPos) on else off
    })
    (renders ++: menu, globals)
  }

  override def resize(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    become(GodClientMenu(core, input))
}
object GodClientMenu {
  def apply(core: ClientCore, input: Input): GodClientMenu = {
    val center = input.windowSize / 2 toInts
    val rad = V2I(150, 18)
    val buttons = Seq(
      MenuButton(center - rad + V2I(0, 50), center + rad + V2I(0, 50), "Resume Game", (c, w, i) => {
        GodClientMain(c.asInstanceOf[GodClientMenu].core) -> Seq(
          CaptureCursor,
          ClientPrint("resuming game")
        )
      }),
      MenuButton(center - rad - V2I(0, 50), center + rad - V2I(0, 50), "Exit Game", (c, w, i) => {
        c.cause(ClientPrint("exiting game"), Exit)
      })
    )
    GodClientMenu(core, buttons, None)
  }
}

/*
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
    val loadTarget = (chunkPos - GodClient.loadRad) toAsSet (chunkPos + GodClient.loadRad)

    cause(
      SetCamPos(input.camPos + (movDir * GodClient.moveSpeed)),
      SetLoadTarget(loadTarget, loadTarget.shroat(4))
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
*/
/*

  val hud = new GodHUD(chat, "", Color.CLEAR)

  override def render(world: RenderWorld, input: Input): (HUD, Seq[RenderUnit]) = {
    val units = new ArrayBuffer[RenderUnit]

    for (v <- world.placeBlock(input.camPos, input.camDir, 64)) {
      units += new BlockOutline(v, Color.WHITE, 0.95f)
    }

    /*
    if (pressed(ALT_LEFT)) {
      for (c <- world.debugLoadedChunks) {
        units += new ChunkOutline(c, Color.WHITE)
      }
      for (c <- world.debugLoadedTerrain) {
        units += new ChunkOutline(c, Color.BLUE)
      }
    }
    */
    input.sessionData.get("chunk_debug_mode").map(_.asInstanceOf[String]).getOrElse("") match {
      case "chunk" => for (c <- world.debugLoadedChunks) {
        units += new ChunkOutline(c, Color.WHITE)
      }
      case "terrain" => for (c <- world.debugLoadedTerrain) {
        units += new ChunkOutline(c, Color.GRAY)
      }
      case _ =>
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
 */

/*
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
*/
/*
case class GodClientMenu(pressed: Set[Int], buttons: Seq[MenuButton], pressing: Option[MenuButton], chat: Chat, camPos: V3F, camDir: V3F) extends ClientLogic {

  override def become(replacement: ClientLogic): (ClientLogic, Seq[ClientEffect]) = {
    if (replacement.isInstanceOf[GodClient]) replacement -> Seq(CaptureCursor)
    else super.become(replacement)
  }

  override def tick(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = {
    val chunkPos = input.camPos / 16 toInts
    val loadTarget = (chunkPos - GodClient.loadRad) toAsSet (chunkPos + GodClient.loadRad);
    cause(SetLoadTarget(loadTarget, loadTarget.shroat(4)))
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



  /*
  override def render(world: RenderWorld, input: Input): (HUD, Seq[RenderUnit]) = {
    hud -> Seq.empty
  }
  */

  override def render(world: RenderWorld, input: Input): (Seq[Render[_ <: Shader]], GlobalRenderData) =
    world.renderableChunks.flatMap(_.renderables) ++ buttons.flatMap(
      b => if (b contains V2I(Gdx.input.getX, Gdx.input.getY)) b.components._2 else b.components._1
    ) -> GlobalRenderData(camPos, camDir, )

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

  override def render(world: RenderWorld, input: Input): (HUD, Seq[RenderUnit]) = (hud, Seq.empty)
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
    val loadTarget = (chunkPos - GodClient.loadRad) toAsSet (chunkPos + GodClient.loadRad)

    cause(
      SetCamPos(input.camPos + (movDir * GodClient.moveSpeed)),
      SetLoadTarget(loadTarget, loadTarget.shroat(4))
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

  override def render(world: RenderWorld, input: Input): (HUD, Seq[RenderUnit]) = {
    val units = new ArrayBuffer[RenderUnit]

    for (v <- world.placeBlock(input.camPos, input.camDir, 64)) {
      units += new BlockOutline(v, Color.WHITE, 0.95f)
    }

    /*
    if (pressed(ALT_LEFT)) {
      for (c <- world.debugLoadedChunks) {
        units += new ChunkOutline(c, Color.WHITE)
      }
      for (c <- world.debugLoadedTerrain) {
        units += new ChunkOutline(c, Color.BLUE)
      }
    }
    */
    input.sessionData.get("chunk_debug_mode").map(_.asInstanceOf[String]).getOrElse("") match {
      case "chunk" => for (c <- world.debugLoadedChunks) {
        units += new ChunkOutline(c, Color.WHITE)
      }
      case "terrain" => for (c <- world.debugLoadedTerrain) {
        units += new ChunkOutline(c, Color.GRAY)
      }
      case _ =>
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
*/
object GodClient {
  val turnSpeed = 0.25f
  val moveSpeed = 1f
  val margin = 1f
  val loadRad = V3I(12, 5, 12)
}