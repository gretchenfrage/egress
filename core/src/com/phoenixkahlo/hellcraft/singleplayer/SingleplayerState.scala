package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g3d._
import com.badlogic.gdx.graphics.g3d.utils.FirstPersonCameraController
import com.badlogic.gdx.utils.Pool
import com.badlogic.gdx.{Gdx, InputAdapter, InputMultiplexer}
import com.phoenixkahlo.hellcraft.core.entity.{Cube, CubeFrame, GlideCube, SoundCube}
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState}
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.graphics.models.{BlockOutline, ChunkOutline}
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.menu.MainMenu
import com.phoenixkahlo.hellcraft.util.audio.AudioUtil
import com.phoenixkahlo.hellcraft.util.caches.Cache
import com.phoenixkahlo.hellcraft.util.collections.DependencyGraph
import com.phoenixkahlo.hellcraft.util.threading.UniExecutor
import other.AppDirs

import scala.concurrent.duration._


class SingleplayerState(providedResources: Cache[ResourcePack]) extends GameState with Runnable {

  private var save: AsyncSave = _
  private var clock: GametimeClock = _
  private var infinitum: Infinitum = _
  private var resources: ResourcePack = _
  private var renderer: Renderer = _
  private var controller: FirstPersonCameraController = _
  private var vramGraph: DependencyGraph = _
  private var mainLoopTasks: java.util.Queue[Runnable] = _
  private var updateThread: Thread = _
  private var g = 0

  override def onEnter(driver: GameDriver): Unit = {
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
    val generator = new Generator(res)
    save = new RegionGenAsyncSave(AppDirs.dataDir("egress").resolve("single"), new CarboniteSerialService, generator.genChunk)

    clock = new GametimeClock

    println("instantiating history")
    infinitum = new Infinitum(res, save, 1f / 20f)

    println("loading resources")
    resources = providedResources()

    println("creating renderer")
    renderer = new Renderer(resources)

    println("instantiating controller")
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
        } else false

      override def touchDown(screenX: Int, screenY: Int, pointer: Int, button: Int): Boolean = {
        if (button == 1) {
          val camPos = V3F(renderer.cam.position)
          val camDir = V3F(renderer.cam.direction)
          val world = infinitum()
          mainLoopTasks.add(() => {
            for (v <- world.placeMat(camPos, camDir, 16)) {
              infinitum.update(infinitum().chunks.keySet,
                Seq(SetMat(v, Blocks.Stone, WorldRes, UUID.randomUUID())))
            }
          })
        }
        true
      }
    })
    controller = new FirstPersonCameraController(renderer.cam)
    multiplexer.addProcessor(controller)
    Gdx.input.setInputProcessor(multiplexer)

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

        // update world
        UniExecutor.point = V3F(renderer.cam.position)
        val p = (V3F(renderer.cam.position) / 16).floor
        val effects = infinitum.update(((p - LoadDist) to (p + LoadDist)).toSet)
        val time = infinitum().time

        // process effects
        effects(SoundEffect).map(_.asInstanceOf[SoundEffect])
          .foreach(AudioUtil.play(resources, V3F(renderer.cam.position)))

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
    val (toRender, interpolation) = (infinitum(), NoInterpolation)

    // update controller
    controller.update()

    // get render units
    var units = toRender.renderables(resources)

    // add debug units
    if (Gdx.input.isKeyPressed(Keys.ALT_LEFT)) {
      toRender.chunks.values.map(_.terrain).foreach {
        case t: ProtoTerrain => units +:= new ChunkOutline(t.pos, Color.RED)
        case t: CompleteTerrain => units +:= new ChunkOutline(t.pos, Color.GREEN)
      }
      infinitum.loading.foreach(p => {
        units +:= new ChunkOutline(p, Color.WHITE)
      })
    }

    // draw a cube where you're pointing
    for (v <- toRender.placeMat(V3F(renderer.cam.position), V3F(renderer.cam.direction), 16)) {
      units +:= new BlockOutline(v, Color.WHITE)
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

    // convert to provider
    val provider = new RenderableProvider {
      override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit = {
        units.flatMap(_ (interpolation)).foreach(renderables.add)
      }
    }

    // render
    renderer.render(toRender, Seq(provider))
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
    infinitum.finalSave().onComplete(() => println("...saved!"))
    vramGraph.managing.foreach(_.dispose())
    UniExecutor.deactivate()
  }

}
