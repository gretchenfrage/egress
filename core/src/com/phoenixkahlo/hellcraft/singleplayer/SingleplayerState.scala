package com.phoenixkahlo.hellcraft.singleplayer

import java.awt.Toolkit
import java.awt.event.KeyEvent
import java.util.UUID
import java.util.concurrent.{ConcurrentLinkedQueue, ThreadLocalRandom}

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.graphics.g3d._
import com.badlogic.gdx.graphics.g3d.utils.FirstPersonCameraController
import com.badlogic.gdx.utils.Pool
import com.badlogic.gdx._
import com.phoenixkahlo.hellcraft.core.entity._
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.client.ClientSessionData.ClientSessionData
import com.phoenixkahlo.hellcraft.core.client._
import com.phoenixkahlo.hellcraft.core.util.GroupedEffects
import com.phoenixkahlo.hellcraft.fgraphics.{DefaultRenderer, Renderer, ResourcePack}
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState}
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.menu.MainMenu
import com.phoenixkahlo.hellcraft.singleplayer.SingleWorld.ChunkEnts
import com.phoenixkahlo.hellcraft.util.audio.AudioUtil
import com.phoenixkahlo.hellcraft.util.caches.Cache
import com.phoenixkahlo.hellcraft.util.collections.spatial.SpatialTemporalQueue
import com.phoenixkahlo.hellcraft.util.collections._
import com.phoenixkahlo.hellcraft.util.threading.{Fut, Promise, UniExecutor}
import other.AppDirs

import scala.concurrent.duration._


class SingleplayerState(providedResources: Cache[ResourcePack]) extends GameState with Runnable {

  private var driver: GameDriver = _
  private var save: AsyncSave[ChunkEnts] = _
  private var clock: GametimeClock = _
  private var infinitum: SingleContinuum = _
  private var pack: ResourcePack = _
  private var renderer: Renderer = _
  private var processor: InputProcessor = _
  @volatile private var chunkDomain = V3ISet.empty
  @volatile private var terrainDomain = V3ISet.empty
  private var clientLogic: ClientLogic = _
  private var sessionData: ClientSessionData = ClientSessionData.empty
  private val clientLogicQueue = new ConcurrentLinkedQueue[ClientLogic => ((ClientWorld, ClientLogic.Input) => ClientLogic.Output)]
  private val worldEffectQueue = new ConcurrentLinkedQueue[UpdateEffect]
  private var mainLoopTasks = new java.util.concurrent.ConcurrentLinkedQueue[Runnable]
  private var updateThread: Thread = _
  private var g = 0

  override def onEnter(driver: GameDriver): Unit = {
    this.driver = driver
    val res = 16

    println("activating uni executor")
    UniExecutor.activate(
      auxBackgroundThreads,
      task => {
        val thread = new Thread(task, "uni exec thread")
        thread.setPriority(backgroundThreadPriority)
        thread
      }, t => {
        System.err.println("uni executor failure")
        t.printStackTrace()
        driver.enter(new MainMenu(providedResources))
      },
      SpatialTemporalQueue.timeDoesntMatter,
      V3F(1, 1.5f, 1)
    )

    //clientLogic = AvatarClientMain(clientCore, EntID.random[Avatar]())

    println("instantiating save")
    val generator = new DefaultGenerator(res)
    save = new LevelDBSave(AppDirs.dataDir("egress").resolve("single"), generator, ChunkEnts.elevate)

    println("instantiating clock")
    clock = new DefGametimeClock
    //clock = new ManualGametimeClock

    println("instantiating history")
    //infinitum = new Infinitum(res, save, 1f / 20f)
    infinitum = new SingleContinuum(save)

    println("creating client logic")
/*
    clientLogic = GodClientMain(ClientCore(
      Set.empty,
      Chat(Seq("player joined the game")),
      Origin, North
    ))
*/

    val clientCore = ClientCore(
      Set.empty,
      Chat(Seq("player joined the game")),
      Origin, North
    )
    implicit val rand = new MRNG(ThreadLocalRandom.current.nextLong())
    val avatar = Avatar(V3I(8, 50, 8))
    worldEffectQueue.add(PutEnt(avatar))
    clientLogic = AvatarClientMain(clientCore, avatar.id)

    println("setting load target")
    chunkDomain = avatar.chunkPos - V3I(1, 6, 1) toAsSet avatar.chunkPos + V3I(1, 2, 1)
    terrainDomain = chunkDomain.bloat

    /*
    val clientCore = ClientCore(
      Set.empty,
      Chat(Seq("player joined the game")),
      Origin, North
    )
    implicit val rand = new MRNG(ThreadLocalRandom.current.nextLong())
    val avatar = Avatar(V3I(0, 50, 0))
    worldEffectQueue.add(ChunkEvent.putEnt(avatar))
    clientLogic = AvatarClientMain(clientCore, avatar.id)


    println("setting load target")
    chunkDomain = avatar.chunkPos - V3I(1, 6, 1) toAsSet avatar.chunkPos + V3I(1, 2, 1)
    terrainDomain = chunkDomain.bloat
    */

    println("loading resources")
    pack = providedResources()

    println("creating renderer")
    //renderer = new Renderer(pack)
    renderer = new DefaultRenderer(pack)

    println("instantiating input processor")
    processor = new InputProcessor {
      override def touchUp(screenX: Int, screenY: Int, pointer: Int, button: Int): Boolean =
        clientLogicQueue.add(_.touchUp(V2I(screenX, screenY), pointer, Button(button)))

      override def touchDown(screenX: Int, screenY: Int, pointer: Int, button: Int): Boolean =
        clientLogicQueue.add(_.touchDown(V2I(screenX, screenY), pointer, Button(button)))

      override def keyUp(keycode: Int): Boolean =
        clientLogicQueue.add(_.keyUp(keycode))

      override def scrolled(amount: Int): Boolean =
        clientLogicQueue.add(_.scrolled(amount))

      override def keyTyped(character: Char): Boolean =
        clientLogicQueue.add(_.keyTyped(character))

      override def touchDragged(screenX: Int, screenY: Int, pointer: Int): Boolean =
        clientLogicQueue.add(_.touchDragged(V2I(screenX, screenY), V2I(Gdx.input.getDeltaX(pointer), Gdx.input.getDeltaY(pointer)), pointer))

      override def keyDown(keycode: Int): Boolean =
        if (keycode == Input.Keys.ENTER) clock match {
          case manual: ManualGametimeClock => manual.tick(); true
          case _ => clientLogicQueue.add(_.keyDown(keycode))
        }
        else clientLogicQueue.add(_.keyDown(keycode))

      override def mouseMoved(screenX: Int, screenY: Int): Boolean =
        clientLogicQueue.add(_.mouseMoved(V2I(screenX, screenY), V2I(Gdx.input.getDeltaX, Gdx.input.getDeltaY)))
    }
    Gdx.input.setInputProcessor(processor)

    Thread.currentThread().setPriority(renderLoopThreadPriority)

    println("spawning updating thread")
    updateThread = new Thread(this, "update thread")
    updateThread.setPriority(mainLoopThreadPriority)
    clock.reset()
    updateThread.start()

    println("singleplayer state initialized")
  }

  override def run(): Unit = {
    try {
      while (!Thread.interrupted()) {
        // run tasks from queue
        while (mainLoopTasks.size > 0)
          mainLoopTasks.remove().run()

        // accumulate effects
        var externEffects = Seq.empty[UpdateEffect]
        while (worldEffectQueue.size > 0)
          externEffects +:= worldEffectQueue.remove()

        // update world
        UniExecutor.point = V3F(renderer.cam.position)
        val effects = new GroupedEffects(infinitum.update(chunkDomain, terrainDomain, externEffects))
        val time = infinitum.time
        //val time = infinitum().time

        // enqueue input
        clientLogicQueue.add(_.tick)

        // process effects
        effects.bin(SoundEffect).foreach(AudioUtil.play(pack, V3F(renderer.cam.position)))

        val logs = effects.bin(LogEffect)
        if (logs.nonEmpty)
          println("world logs:")
        for (log <- logs)
          println("-- " + log.str)

        //System.gc()

        // manage time
        if (clock.timeSince(time) > (500 milliseconds)) {
          println("can't keep up!")
          clock.forgive(clock.timeSince(time) - (500 milliseconds))
        }
        clock.waitUntil(time + 1)
      }
    } catch {
      case e: InterruptedException => println("singleplayer shutting down")
    }
  }

  override def render(): Unit = {
    // setup
    Gdx.graphics.setTitle(Gdx.graphics.getFramesPerSecond.toString)
    g += 1

    // get world
    val (time, world) = infinitum.timeAndCurr

    // update controller
    val clientInput = new ClientLogic.Input {
      override val camRange: (Float, Float) = (renderer.cam.near, renderer.cam.far)
      override val camPos: V3F = V3F(renderer.cam.position)
      override val camDir: V3F = V3F(renderer.cam.direction)
      override val cursorPos: V2I = V2I(Gdx.input.getX, Gdx.input.getY)
      override val isCursorCaught: Boolean = Gdx.input.isCursorCatched
      override val currentRes: V2I = V2I(Gdx.graphics.getWidth, Gdx.graphics.getHeight)
      override val nanoTime: Long = System.nanoTime()
      override val sessionData: ClientSessionData = SingleplayerState.this.sessionData
      override val pack: ResourcePack = SingleplayerState.this.pack
      override val executor: UniExecutor = UniExecutor.getService
      override def dt: Float = Gdx.graphics.getDeltaTime
      override def keyToChar(keycode: Int): Option[Char] = {
        val str = keycode match {
          case Keys.SPACE => " "
          case Keys.TAB => "\t"
          case Keys.ENTER => "\n"
          case k => Input.Keys.toString(keycode)
        }
        if (str.size == 1) {
          val char = str.head
          if (char >= 0x20 && char <= 0x126) {
            val shift = Gdx.input.isKeyPressed(Keys.SHIFT_LEFT) || Gdx.input.isKeyPressed(Keys.SHIFT_RIGHT)
            Some(if (!shift) char.toLower
            else if (Character.isAlphabetic(char)) char.toUpper
            else char match {
              case '`' => '~'
              case '1' => '!'
              case '2' => '@'
              case '3' => '#'
              case '4' => '$'
              case '5' => '%'
              case '6' => '^'
              case '7' => '&'
              case '8' => '*'
              case '9' => '('
              case '0' => ')'
              case '-' => '_'
              case '=' => '+'
              case '[' => '{'
              case ']' => '}'
              case '\\' => '|'
              case ';' => ':'
              case ''' => '"'
              case ',' => '<'
              case '.' => '>'
              case '/' => '?'
              case _ => '�'
            })
          } else  {
            None
          }
        } else {
          None
        }
      }

    }
    val interp = 1 - clock.fractionalTicksSince(time)
    val renderWorld = world.renderable(clock.fgametime, Trig.clamp(interp, 0, 1))

    var renderOutput: ClientLogic.RenderOutput = null
    clientLogicQueue.add(logic => (world, input) => {
      val (o, r) = logic.frame(renderWorld, input)
      renderOutput = r
      o
    })

    while (clientLogicQueue.size > 0) {
      val (newClientLogic, effects) = clientLogicQueue.remove()(clientLogic)(world, clientInput)
      clientLogic = newClientLogic
      effects.foreach {
        case CauseUpdateEffect(worldEffects) => worldEffects.foreach(worldEffectQueue.add)
        case SetLoadTarget(c, t) =>
          chunkDomain = c
          terrainDomain = t
        case CaptureCursor => Gdx.input.setCursorCatched(true)
        case ReleaseCursor => Gdx.input.setCursorCatched(false)
        case ClientPrint(str) => println(str)
        case Exit => driver.enter(new MainMenu(providedResources))
        case SetSessionProperty(k, v) => sessionData += k -> v
      }
    }
    renderer.cam.update(true)

    val (renders, globals) = renderOutput
    renderer(renders, globals)
  }

  override def onResize(width: Int, height: Int): Unit = {
    renderer.onResize(width, height)
    clientLogicQueue.add(_.resize)
  }

  override def onExit(): Unit = {
    // unhook further input events to avoid problems
    Gdx.input.setInputProcessor(new InputAdapter)
    // start interrupting the main loop
    updateThread.interrupt()
    // while that's closing, we can dispose of the graphics system
    renderer.close()
    // before we resume, we need to wait for the main loop to completely close
    updateThread.join()
    // start saving the world
    println("saving...")
    val close: Promise = infinitum.close()
    // while that's happening, we can dispose of all graphics resources owned by the VRAM graph
    // now we wait for the world to finish saving
    close.await
    println("...saved!")
    // finally, kill the executor, any tasks in it are no longer needed
    UniExecutor.deactivate()
  }

}
