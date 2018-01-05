package com.phoenixkahlo.hellcraft.core.client
import java.util.UUID
import java.util.concurrent.ThreadLocalRandom

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.Texture
import com.badlogic.gdx.math.Matrix4
import com.phoenixkahlo.hellcraft.core.graphics._
import com.phoenixkahlo.hellcraft.core.{Blocks, ChunkEvent, Materials, RenderWorld, World}
import com.phoenixkahlo.hellcraft.fgraphics.ParticleShader.Particle
import com.phoenixkahlo.hellcraft.fgraphics._
import com.phoenixkahlo.hellcraft.math.MatrixFactory.{Rotate, Translate}

import scala.collection.immutable.IndexedSeq
import scala.util.Random
import com.phoenixkahlo.hellcraft.core.client.ClientLogic.Input
import com.phoenixkahlo.hellcraft.math._
import com.badlogic.gdx.Input.Keys._
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.{BitmapFont, GlyphLayout, TextureRegion}
import com.phoenixkahlo.hellcraft.core.client.ClientSessionData._
import com.phoenixkahlo.hellcraft.core.eval.GEval.GEval
import com.phoenixkahlo.hellcraft.core.eval.{ExecCheap, GEval}
import com.phoenixkahlo.hellcraft.fgraphics.hud.{DownLeft, ImgHUDComponent, StrBoxHUDComponent, StrHUDComponent}
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.math
import com.phoenixkahlo.hellcraft.util.caches.ParamCache
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc

import scala.concurrent.duration._
import scala.collection.mutable.ArrayBuffer

case class MenuButton(range: V2I => (V2I, V2I), str: String, onclick: (ClientLogic, World, ClientLogic.Input) => ClientLogic.Output) {
  def contains(v: V2I, input: Input): Boolean = {
    val (min, max) = range(input.currentRes)
    val v2 = V2I(v.xi, input.currentRes.yi - v.yi)
    v2 >= min && v2 <= max
  }
  
  val components: (Seq[Render[HUDShader]], Seq[Render[HUDShader]]) = {
    implicit val exec = ExecCheap
    val frameOff = for {
      pack <- GEval.resourcePack
      (min, max) <- GEval.res.map(range)
    } yield ImgHUDComponent(
      new TextureRegion(pack.frame(MenuPatchPID, 18, max.xi - min.xi, max.yi - min.yi)),
      min,
      max - min
    )
    val frameOn = for {
      pack <- GEval.resourcePack
      (min, max) <- GEval.res.map(range)
    } yield ImgHUDComponent(
      new TextureRegion(pack.frame(MenuPatchActivePID, 18, max.xi - min.xi, max.yi - min.yi)),
      min,
      max - min
    )
    val text = for {
      pack <- GEval.resourcePack
      (min, max) <- GEval.res.map(range)
    } yield {
      val font = pack.font(ButtonFID)
      val layout = new GlyphLayout
      layout.setText(font, str)
      StrHUDComponent(
        str, font,
        (min + max) / 2 + V2F(-layout.width / 2, layout.height / 2),
        Color.BLACK
      )
    }
    (
      Seq(frameOff, text).map(Renderable[HUDShader](_)).map(Render[HUDShader](_, (): Unit)),
      Seq(frameOn, text).map(Renderable[HUDShader](_)).map(Render[HUDShader](_, (): Unit))
    )
  }
}

case class Cloud(pos: V3F, ren: Renderable[TerrainShader]) {
  def render(interp: Float, speed: Float): Render[TerrainShader] =
    Render[TerrainShader](ren, Offset(pos + (East * speed * interp)))

  def move(speed: Float): Cloud = {
    copy(pos = pos + (East * speed))
  }
}
case class ClientCore(pressed: Set[KeyCode], chat: Chat, camPos: V3F, camDir: V3F, clouds: Seq[Cloud] = Seq.empty) {
  def update(world: World, input: Input): ClientCore = {
    val numClouds = 75
    val cloudDist = Repeated(input.camRange._2 + 300)
    val filteredClouds = clouds.filter({ case Cloud(pos, _) => pos > input.camPos - cloudDist && pos < camPos + cloudDist })
    val random = new Random()
    val positions = RNG.v3fs(RNG(random.nextLong)).map(p => ((p * 2 - Ones) * cloudDist.x).copy(y = 400))
    val ordinals = RNG.ints(RNG(random.nextLong))
    val speed = input.sessionData(CloudSpeed) * input.dt
    val addedClouds =
      filteredClouds ++ (positions zip ordinals)
        .map({ case (pos, ord) => Cloud(pos, Clouds.cloud(ord)) })
        .take(numClouds - filteredClouds.size)
    val movedClouds = addedClouds.map(_.move(speed))
    copy(clouds = movedClouds)
  }

  def render(world: RenderWorld, input: Input): (Seq[Render[_ <: Shader]], GlobalRenderData) = {
    implicit val exec = ExecCheap

    // begin accumulating renders
    val renders = new ArrayBuffer[Render[_ <: Shader]]

    // render the chunks
    for (render <- world.renderableChunks.flatMap(_.render(world))) {
      renders += render
    }

    // render the block outline
    for (v <- world.placeBlock(input.camPos, input.camDir, 64)) {
      renders += Render[LineShader](CubeOutline.block(V4I.ones), Offset(v))
    }

    // do some debug renders
    val chunkOutline = CubeOutline.chunk(V4F(1, 1, 1, 1))
    input.sessionData.get(ChunkDebugMode).getOrElse("") match {
      case "chunk" => for (c <- world.debugLoadedChunks) {
        renders += Render[LineShader](chunkOutline, Offset(c * 16))
      }
      case "terrain" => for (c <- world.debugLoadedTerrain) {
        renders += Render[LineShader](chunkOutline, Offset(c * 16))
      }
      case _ =>
    }
    if (input.sessionData.get(ShowTasks).exists(identity)) {
      val (tasks3D, tasks2D, tasksDB3D) = input.executor.getSpatialTasks
      val task3DRenderable = FreeCube(FreeCubeParams(GrayTID, V4F(1, 1, 1, 1)))
      for (p <- tasks3D) {
        renders += Render[GenericShader](task3DRenderable, Offset(p))
      }
      val task2DRenderable = FreeCube(FreeCubeParams(GrayTID, V4F(0, 0, 1, 1)))
      for (p <- tasks2D.map(_.inflate(0))) {
        renders += Render[GenericShader](task2DRenderable, Offset(p))
      }
      val taskDB3DRenderable = FreeCube(FreeCubeParams(GrayTID, V4F(0, 1, 0, 1)))
      for (p <- tasksDB3D) {
        renders += Render[GenericShader](taskDB3DRenderable, Offset(p))
      }
    }

    // render the sky
    // some cosmic data
    val skyDist: Float = input.camRange._2 - 5
    val cycle = world.ftime / (input.sessionData(DayCycle).toSeconds * 20) % 1
    val sunDir = V3F(-Trig.cos(cycle * 360), Trig.sin(cycle * 360), 0)
    val sunPos = camPos + (sunDir * skyDist)
    val moonPos = camPos + (sunDir * -1 * skyDist)
    // compute sky color
    val dayColor = V3F(0.5089f, 0.6941f, 1f)
    val nightColor = V3F(0.0039f, 0.0471f, 0.1843f)
    val transFrac = 0.04f
    var trans: Float = 0
    var from: V3F = null
    var to: V3F = null
    if (cycle < transFrac / 2 || cycle > 1 - transFrac / 2) {
      // sunrise
      trans = (cycle + transFrac / 2) % 1 / transFrac
      from = nightColor
      to = dayColor
    } else if (cycle > 0.5f - transFrac / 2 && cycle < 0.5f + transFrac / 2) {
      // sunset
      trans = (cycle + transFrac / 2 - 0.5f) / transFrac
      from = dayColor
      to = nightColor
    } else if (cycle < 0.5f) {
      // day
      from = dayColor
      to = dayColor
    } else {
      // night
      from = nightColor
      to = nightColor
    }
    val skyColor = ((to - from) * trans) + from
    // compute sun power
    val fromPow = if (from == dayColor) 1 else -1
    val toPow = if (to == dayColor) 1 else -1
    val sunlightPow = ((toPow - fromPow) * trans) + fromPow
    val (lightPow, lightPos) =
      if (sunlightPow > 0) (sunlightPow, sunPos)
      else (0.1f, moonPos)
    // sky render
    val skyParams = SkyParams(skyDist)
    val skyTrans = MatrixFactory(Rotate(South, cycle * 360), Translate(camPos))
    renders += Render[ParticleShader](SunMoon(skyParams), ParticleShader.Params(skyTrans))
    renders += Render[ParticleShader](Stars(skyParams), ParticleShader.Params(skyTrans, V4F(1, 1, 1, Trig.clamp(-sunlightPow, 0, 1))))
    // clouds render
    renders ++= clouds.map(_.render(world.interp, input.sessionData(CloudSpeed) * input.dt))

    // build the globals and return
    val globals = GlobalRenderData(camPos, camDir, lightPos, lightPow, skyColor.inflate(1), 90)
    renders -> globals
  }

  def hud(buffer: String, chatBGColor: Color, input: Input): Seq[Render[HUDShader]] = {
    implicit val exec = ExecCheap
    val crosshair = Renderable[HUDShader](for {
      pack <- GEval.resourcePack
      res <- GEval.res
    } yield ImgHUDComponent(
      pack(CrosshairTID),
      res / 2 - V2F(15, 15),
      V2F(30, 30)
    ))
    val chatbg = Renderable[HUDShader](for {
      dot <- GEval.dot(V4F(chatBGColor))
      res <- GEval.res
    } yield ImgHUDComponent(
      new TextureRegion(dot),
      Origin2D,
      res ** V2F(0.4f, 0.5f)
    ))
    // lol imagine if I used this syntax everywhere, that'd be terrible
    // but I wanted to do this just once
    val text = ("" /: (chat.messages :+ buffer)) (_ + _ + "\n") dropRight 1
    val chattext = Renderable[HUDShader](for {
      pack <- GEval.resourcePack
      res <- GEval.res
    } yield StrBoxHUDComponent(
      text, pack.font(ChatFID),
      Origin2D, DownLeft, res ** V2F(0.4f, 0.5f),
      new Color(1f, 1f, 1f, 0.5f), 3, 10
    ))

    Seq(
      Render[HUDShader](crosshair, (): Unit, mustRender = true),
      Render[HUDShader](chatbg, (): Unit, mustRender = true),
      Render[HUDShader](chattext, (): Unit, mustRender = true)
    )
  }
}

case class GodClientMain(core: ClientCore) extends ClientLogic {
  val rands = ThreadLocal.withInitial[MRNG](() => new MRNG(ThreadLocalRandom.current.nextLong()))
  implicit def rand: MRNG = rands.get

  override def become(replacement: ClientLogic): (ClientLogic, Seq[ClientEffect]) = {
    if (!replacement.isInstanceOf[GodClientMain]) replacement -> Seq(ReleaseCursor)
    else super.become(replacement)
  }



  /*
  override def update(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = {
    var movDir: V3F = Origin
    if (core.pressed(W)) movDir += core.camDir
    if (core.pressed(S)) movDir -= core.camDir
    if (core.pressed(D)) movDir += (core.camDir cross Up).normalize
    if (core.pressed(A)) movDir -= (core.camDir cross Up).normalize

    val chunkPos = core.camPos / 16 toInts
    val loadTarget = (chunkPos - input.sessionData(LoadDist)) toAsSet (chunkPos + input.sessionData(LoadDist))

    val newPos = core.camPos + (movDir * input.sessionData(Speed) * input.dt)

    GodClientMain(core.copy(camPos = newPos).update(world, input)) -> Seq(SetLoadTarget(loadTarget, loadTarget.shroat(4)))
  }
  */

  override def frame(world: RenderWorld, input: Input) =
    ({
      var movDir: V3F = Origin
      if (core.pressed(W)) movDir += core.camDir
      if (core.pressed(S)) movDir -= core.camDir
      if (core.pressed(D)) movDir += (core.camDir cross Up).normalize
      if (core.pressed(A)) movDir -= (core.camDir cross Up).normalize

      val chunkPos = core.camPos / 16 toInts
      val loadTarget = (chunkPos - input.sessionData(LoadDist)) toAsSet (chunkPos + input.sessionData(LoadDist))

      val newPos = core.camPos + (movDir * input.sessionData(Speed) * input.dt)

      GodClientMain(core.copy(camPos = newPos).update(world, input)) -> Seq(SetLoadTarget(loadTarget, loadTarget.shroat(4)))
    }, {
      val (wrender, globals) = core.render(world, input)
      val hud = core.hud("", Color.CLEAR, input)
      (wrender ++ hud, globals)
    })

  override def keyDown(keycode: KeyCode)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = keycode match {
    case ESCAPE => become(GodClientMenu(core, input))
    case T => become(GodClientChat(core))
    case SLASH => become(GodClientChat(core, buffer = "/"))
    case _ => become(GodClientMain(core.copy(pressed = core.pressed + keycode)))
  }

  override def keyUp(keycode: KeyCode)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    become(GodClientMain(core.copy(pressed = core.pressed - keycode)))

  override def touchDown(pos: V2I, pointer: Int, button: Button)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    if (input.isCursorCaught) button match {
      case Right =>
        world
          .placeBlock(core.camPos, core.camDir, 64)
          .map(v => cause(CauseUpdateEffect(ChunkEvent.setMat(v, Blocks.Brick, meshBlocksFast = true))))
          .getOrElse(nothing)
      case _ => nothing
    } else cause(CaptureCursor)

  override def mouseMoved(pos: V2I, delta: V2I)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = {
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

      become(GodClientMain(core.copy(camDir = camDir)))
    } else nothing
  }

  /*
  override def render(world: RenderWorld, input: Input): (Seq[Render[_ <: Shader]], GlobalRenderData) = {
    val (wrender, globals) = core.render(world, input)
    val hud = core.hud("", Color.CLEAR, input)
    (wrender ++ hud, globals)
  }
  */

}

case class GodClientMenu(core: ClientCore, buttons: Seq[MenuButton], pressing: Option[MenuButton]) extends ClientLogic {
  override def become(replacement: ClientLogic): (ClientLogic, Seq[ClientEffect]) = {
    if (replacement.isInstanceOf[GodClientMain]) replacement -> Seq(CaptureCursor)
    else super.become(replacement)
  }

  override def tick(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = {
    val chunkPos = core.camPos / 16 toInts
    val loadTarget = (chunkPos - input.sessionData(LoadDist)) toAsSet (chunkPos + input.sessionData(LoadDist))
    cause(SetLoadTarget(loadTarget, loadTarget.shroat(4)))
  }

  override def keyDown(keycode: KeyCode)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    if (keycode == ESCAPE) become(GodClientMain(core))
    else become(copy(core = core.copy(pressed = core.pressed + keycode)))


  override def touchDown(pos: V2I, pointer: KeyCode, button: Button)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    buttons.reverse.find(_ contains (pos, input)).map(button => become(copy(pressing = Some(button)))).getOrElse(nothing)

  override def touchUp(pos: V2I, pointer: KeyCode, button: Button)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    pressing match {
      case Some(pressing) => buttons.reverse.find(_ contains (pos, input)).filter(_ == pressing).map(_.onclick(this, world, input)).getOrElse(nothing)
      case None => nothing
    }



  /*
  override def render(world: RenderWorld, input: Input): (Seq[Render[_ <: Shader]], GlobalRenderData) = {
    val (renders, globals) = core.render(world, input)
    val menu: Seq[Render[HUDShader]] = buttons.flatMap(b => b.components match {
      case (off, on) => if (b contains (input.cursorPos, input)) on else off
    })
    (renders ++: menu, globals)
  }
  */

  //override def resize(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
  //  become(GodClientMenu(core, input))
  override def frame(world: RenderWorld, input: Input) = {
    val (renders, globals) = core.render(world, input)
    val menu: Seq[Render[HUDShader]] = buttons.flatMap(b => b.components match {
      case (off, on) => if (b contains (input.cursorPos, input)) on else off
    })
    (nothing, (renders ++: menu, globals))
  }
}
object GodClientMenu {
  def apply(core: ClientCore, input: Input): GodClientMenu = {
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
    GodClientMenu(core, buttons, None)
  }
}

case class GodClientChat(core: ClientCore, cursor: Boolean = true, buffer: String = "") extends ClientLogic {
  override def become(replacement: ClientLogic): (ClientLogic, Seq[ClientEffect]) =
    if (replacement.isInstanceOf[GodClientMain]) replacement -> Seq(CaptureCursor)
    else super.become(replacement)

  def shouldCursor(input: Input): Boolean = {
    val interval = 1000000000
    input.nanoTime % interval > interval / 2
  }

  /*
  override def update(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    if (shouldCursor(input) ^ cursor) become(copy(cursor = shouldCursor(input)))
    else nothing
    */

  override def keyDown(keycode: KeyCode)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) = keycode match {
    case ESCAPE => become(GodClientMain(core))
    case DEL => become(copy(buffer = buffer.dropRight(1)))
    case ENTER =>
      val (newChat, effects) = core.chat + (buffer, world, input)
      GodClientMain(core.copy(chat = newChat)) -> (effects :+ CaptureCursor)
    case _ => input.keyToChar(keycode) match {
      case Some(c) => become(copy(buffer = buffer + c))
      case None => nothing
    }
  }



  /*
  override def render(world: RenderWorld, input: Input): (Seq[Render[_ <: Shader]], GlobalRenderData) = {
    val (wrender, globals) = core.render(world, input)
    val hud = core.hud(
      if (cursor) buffer + "|" else buffer,
      new Color(0.1718f, 0.2422f, 0.3125f, 0.3f),
      input
    )
    (wrender ++ hud, globals)
  }
  */
  override def frame(world: RenderWorld, input: Input) = {
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

object GodClient {
  //val turnSpeed = 0.25f
  //val moveSpeed = 1f
  val margin = 1f
  //val loadDist = "load_dist"
  //val dayDuration = "day_duration"
  //val moveSpeed = "move_speed"
  //val loadRad = V3I(12, 5, 12)
  //val dayDuration = 10 seconds
  //val dayTicks = dayDuration.toSeconds * 20
}