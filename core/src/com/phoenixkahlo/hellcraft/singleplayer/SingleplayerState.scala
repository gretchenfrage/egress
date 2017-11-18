package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID
import java.util.concurrent.{ConcurrentLinkedQueue, ThreadLocalRandom}

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.graphics.g3d._
import com.badlogic.gdx.graphics.g3d.utils.FirstPersonCameraController
import com.badlogic.gdx.utils.Pool
import com.badlogic.gdx.{Gdx, InputAdapter, InputMultiplexer, InputProcessor}
import com.phoenixkahlo.hellcraft.core.entity._
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.client._
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState}
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.graphics.models.{BlockOutline, ChunkOutline}
import com.phoenixkahlo.hellcraft.graphics.shaders.ParticleSID
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.menu.MainMenu
import com.phoenixkahlo.hellcraft.util.audio.AudioUtil
import com.phoenixkahlo.hellcraft.util.caches.Cache
import com.phoenixkahlo.hellcraft.util.collections.{DependencyGraph, ResourceNode}
import com.phoenixkahlo.hellcraft.util.threading.UniExecutor
import other.AppDirs

import scala.concurrent.duration._


class SingleplayerState(providedResources: Cache[ResourcePack]) extends GameState with Runnable {

  private var driver: GameDriver = _
  private var save: AsyncSave = _
  private var clock: GametimeClock = _
  private var infinitum: Infinitum = _
  private var resourcePack: ResourcePack = _
  private var renderer: Renderer = _
  private var controller: InputProcessor = _
  @volatile private var loadTarget = Set.empty[V3I]
  //private var controller: FirstPersonCameraController = _
  private var clientLogic: ClientLogic = GodClient(Set.empty)
  private val clientLogicQueue = new ConcurrentLinkedQueue[ClientLogic => ((World, ClientLogic.Input) => ClientLogic.Output)]
  private val worldEffectQueue = new ConcurrentLinkedQueue[UpdateEffect]
  private var vramGraph: DependencyGraph = _
  private var mainLoopTasks: java.util.Queue[Runnable] = _
  private var updateThread: Thread = _
  private var g = 0

  override def onEnter(driver: GameDriver): Unit = {
    this.driver = driver
    val res = WorldRes

    println("activating uni executor")
    UniExecutor.activate(auxBackgroundThreads, task => {
      val thread = new Thread(task, "uni exec thread")
      thread.setPriority(backgroundThreadPriority)
      thread
    }, t => {
      System.err.println("uni executor failure")
      t.printStackTrace()
      driver.enter(new MainMenu(providedResources))
    }, 4)

    println("instantiating save")
    val generator = new DefaultGenerator(res)
    //save = new RegionGenAsyncSave(AppDirs.dataDir("egress").resolve("single"), new CarboniteSerialService, generator.chunkAt)
    save = new LevelDBSave(AppDirs.dataDir("egress").resolve("single"), generator)

    clock = new GametimeClock

    println("instantiating history")
    infinitum = new Infinitum(res, save, 1f / 20f)

    println("loading resources")
    resourcePack = providedResources()

    println("creating renderer")
    renderer = new Renderer(resourcePack)

    println("instantiating controller")
    /*
    val multiplexer = new InputMultiplexer
    multiplexer.addProcessor(new InputAdapter {
      override def keyDown(keycode: Int): Boolean =
        if (keycode == Keys.ESCAPE) {
          println("closing world")
          driver.enter(new MainMenu(providedResources))
          true
        } else if (keycode == Keys.J) {
          println(V3F(renderer.cam.direction).direction)
          true
        } else if (keycode == Keys.P) {
          val camPos = V3F(renderer.cam.position)
          val camDir = V3F(renderer.cam.direction)
          val world = infinitum()
          mainLoopTasks.add(() => {
            val hit = world.rayhit(camPos, camDir)
            println("hit = " + hit)
            if (hit.isDefined)
              infinitum.update(infinitum().chunks.keySet, Seq(PutEntity(new Cube(StoneTID, hit.get, UUID.randomUUID()), UUID.randomUUID())))

          })
          true

        } else if (keycode == Keys.H) {
          val camPos = V3F(renderer.cam.position)
          val camDir = V3F(renderer.cam.direction)
          val world = infinitum()
          mainLoopTasks.add(() => {
            world.seghit(camPos, camDir, 16).foreach(
              v => infinitum.update(infinitum().chunks.keySet,
                Seq(PutEntity(SoundCube(SnapSID, 60, v, UUID.randomUUID()), UUID.randomUUID())))
            )
          })
          true
        } else if (keycode == Keys.Y) {
          val camPos = V3F(renderer.cam.position)
          val camDir = V3F(renderer.cam.direction)
          val world = infinitum()
          mainLoopTasks.add(() => {
            world.rayhit(camPos, camDir).foreach(
              v => infinitum.update(infinitum().chunks.keySet,
                Seq(PutEntity(GlideCube(Ones, v, UUID.randomUUID()), UUID.randomUUID())))
            )
          })
          true
        } else if (keycode == Keys.L) {
          val camPos = V3F(renderer.cam.position)
          val camDir = V3F(renderer.cam.direction)
          val world = infinitum()
          mainLoopTasks.add(() => {
            for (v <- world.rayhit(camPos, camDir)) {
              infinitum.update(infinitum().chunks.keySet, Seq(
                PutEntity(PhysCube(camDir.normalize, v + (Up * 10), UUID.randomUUID(), North), UUID.randomUUID())
              ))
            }
          })
          true
        } else if (keycode == Keys.G) {
          val camPos = V3F(renderer.cam.position)
          val camDir = V3F(renderer.cam.direction)
          val world = infinitum()
          mainLoopTasks.add(() => {
            for (v <- world.rayhit(camPos, camDir)) {
              infinitum.update(infinitum().chunks.keySet, Seq(
                PutEntity(GhostCube(v, UUID.randomUUID()), UUID.randomUUID())
              ))
            }
          })
          true
        } else if (keycode == Keys.F) {
          val camPos = V3F(renderer.cam.position)
          val camDir = V3F(renderer.cam.direction)
          val world = infinitum()
          mainLoopTasks.add(() => {
            for (v <- world.rayhit(camPos, camDir)) {
              infinitum.update(infinitum().chunks.keySet, Seq(
                PutEntity(PreGenCloud(v, ThreadLocalRandom.current().nextInt(), UUID.randomUUID()), UUID.randomUUID())
              ))
            }
          })
          true
        } else if (keycode == Keys.R) {
          val camPos = V3F(renderer.cam.position)
          val camDir = V3F(renderer.cam.direction)
          val world = infinitum()
          mainLoopTasks.add(() => {
            for (v <- world.rayhit(camPos, camDir)) {
              infinitum.update(infinitum().chunks.keySet, Seq(
                PutEntity(new ProceduralCloud(v, ThreadLocalRandom.current().nextLong(), V3I(20, 15, 30), 0.7f, UUID.randomUUID(), 3.5f, 6f, 15, Repeated(1.5f), null), UUID.randomUUID())
              ))
            }
          })
          true
        } else false



      override def touchDown(screenX: Int, screenY: Int, pointer: Int, button: Int): Boolean = {
        if (button == 1) {
          val camPos = V3F(renderer.cam.position)
          val camDir = V3F(renderer.cam.direction)
          val p = camPos / 16 floor
          val world = infinitum()
          mainLoopTasks.add(() => {
            for (v <- world.placeBlock(camPos, camDir, 16)) {
              infinitum.update(infinitum().chunks.keySet,
                Seq(SetMat(v, Blocks.Brick, WorldRes, UUID.randomUUID(), revalBlocks = true)))
            }
          })
        }
        true
      }
    })
    controller = new FirstPersonCameraController(renderer.cam)
    multiplexer.addProcessor(controller)
    Gdx.input.setInputProcessor(multiplexer)
    */

    controller = new InputProcessor {
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
        clientLogicQueue.add(_.keyDown(keycode))

      override def mouseMoved(screenX: Int, screenY: Int): Boolean =
        clientLogicQueue.add(_.mouseMoved(V2I(screenX, screenY), V2I(Gdx.input.getDeltaX, Gdx.input.getDeltaY)))
    }
    Gdx.input.setInputProcessor(controller)

    println("instantiating VRAM graph")
    vramGraph = new DependencyGraph

    Thread.currentThread().setPriority(renderLoopThreadPriority)

    println("spawning updating thread")
    mainLoopTasks = new java.util.concurrent.ConcurrentLinkedQueue
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
        val p = (V3F(renderer.cam.position) / 16).floor
        val effects = infinitum.update(/*((p - LoadDist) to (p + LoadDist)).toSet*/loadTarget, externEffects)
        val time = infinitum().time

        // enqueue input
        clientLogicQueue.add(_.tick)

        // process effects
        effects(SoundEffect).map(_.asInstanceOf[SoundEffect])
          .foreach(AudioUtil.play(resourcePack, V3F(renderer.cam.position)))

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
    Gdx.graphics.setTitle(Gdx.graphics.getFramesPerSecond.toString)

    // setup
    g += 1

    // interpolation
    val (toRender: SWorld, interpolation: Interpolation) = (infinitum(), NoInterpolation)

    // update controller
    //controller.update()
    val clientInput = new ClientLogic.Input {
      override def camPos: V3F = V3F(renderer.cam.position)
      override def camDir: V3F = V3F(renderer.cam.direction)
      override def isCursorCaught: Boolean = Gdx.input.isCursorCatched
    }
    clientLogicQueue.add(_.update)
    while (clientLogicQueue.size > 0) {
      val (newClientLogic, effects) = clientLogicQueue.remove()(clientLogic)(toRender, clientInput)
      clientLogic = newClientLogic
      effects.foreach {
        case CauseUpdateEffect(worldEffects) => worldEffects.foreach(worldEffectQueue.add)
        case SetLoadTarget(target) => loadTarget = target
        case SetCamPos(p) => renderer.cam.position.set(p.x, p.y, p.z)
        case SetCamDir(d) => renderer.cam.direction.set(d.x, d.y, d.z)
        case SetCamFOV(fov) => renderer.cam.fieldOfView = fov
        case CaptureCursor => Gdx.input.setCursorCatched(true)
        case ReleaseCursor => Gdx.input.setCursorCatched(false)
        case Exit => driver.enter(new MainMenu(providedResources))
      }
    }
    renderer.cam.update(true)

    // get render units
    var units = toRender.renderables(resourcePack)

    // add debug units
    if (Gdx.input.isKeyPressed(Keys.ALT_LEFT)) {
      val (complete, incomplete) = toRender.chunks.values.partition(_.isComplete)
      for (chunk <- complete) {
        units +:= new ChunkOutline(chunk.pos, Color.GREEN)
      }
      for (chunk <- incomplete) {
        units +:= new ChunkOutline(chunk.pos, Color.RED)
      }
    }
    val (tasks3D, tasks2D, tasksDB3D) = UniExecutor.getService.getSpatialTasks
    for (p <- tasks3D) {
      units +:= CubeRenderer(GrayTID, Color.WHITE, p)(resourcePack)
    }
    for (p <- tasks2D.map(_.inflate(0))) {
      units +:= CubeRenderer(GrayTID, Color.BLUE, p)(resourcePack)
    }
    for (p <- tasksDB3D) {
      units +:= CubeRenderer(GrayTID, Color.GREEN, p)(resourcePack)
    }


    // draw a cube where you're pointing
    for (v <- toRender.placeBlock(V3F(renderer.cam.position), V3F(renderer.cam.direction), 16)) {
      units +:= new BlockOutline(v / WorldRes * 16, Color.WHITE, scale = 16f / WorldRes * 0.95f)
    }

    // do memory management
    val nodes = units.par.flatMap(_.resources).seq
    vramGraph ++= nodes
    if (g % 600 == 0) UniExecutor.exec(() => {
      val garbage = vramGraph.garbage(nodes)
      Gdx.app.postRunnable(() => {
        println("deleting " + garbage.size + " resource nodes")
        garbage.foreach(_.dispose())
        vramGraph --= garbage.toSeq
      })
    })

    // render
    renderer.render(toRender, units, interpolation)
  }

  override def onResize(width: Int, height: Int): Unit = {
    renderer.onResize(width, height)
  }

  override def onExit(): Unit = {
    Gdx.input.setInputProcessor(new InputAdapter)
    updateThread.interrupt()
    renderer.dispose()
    updateThread.join()
    println("saving...")
    val close = infinitum.finalSave()
    vramGraph.managing.foreach(_.dispose())
    close.await
    println("...saved!")
    UniExecutor.deactivate()
  }

}
