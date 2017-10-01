package com.phoenixkahlo.hellcraft.singleplayer

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g3d._
import com.badlogic.gdx.graphics.g3d.utils.FirstPersonCameraController
import com.badlogic.gdx.utils.Pool
import com.badlogic.gdx.{Gdx, InputAdapter, InputMultiplexer}
import com.phoenixkahlo.hellcraft.core.{Densities, Meshable, Vertices}
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState}
import com.phoenixkahlo.hellcraft.graphics.{ChunkOutline, NoInterpolation, ResourcePack}
import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.menu.MainMenu
import com.phoenixkahlo.hellcraft.util.DependencyGraph
import com.phoenixkahlo.hellcraft.util.caches.Cache
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
  private var updateThread: Thread = _
  private var g = 0

  override def onEnter(driver: GameDriver): Unit = {
    val res = 32

    println("activating uni executor")
    UniExecutor.activate(Runtime.getRuntime.availableProcessors() - 2, task => {
      val thread = new Thread(task, "uni exec thread")
      thread.setPriority(backgroundThreadPriority)
      thread
    }, t => {
      System.err.println("uni executor failure")
      t.printStackTrace()
      driver.enter(new MainMenu(providedResources))
    })

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
          println(V3F(renderer.cam.direction))
          true
        } else false

    })
    controller = new FirstPersonCameraController(renderer.cam)
    multiplexer.addProcessor(controller)
    Gdx.input.setInputProcessor(multiplexer)

    println("instantiating VRAM graph")
    vramGraph = new DependencyGraph

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
        // update world
        UniExecutor.point = V3F(renderer.cam.position)
        val p = (V3F(renderer.cam.position) / 16).floor
        infinitum.update(((p - LoadDist) to (p + LoadDist)).toSet)
        val time = infinitum().time

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
        case Densities(p, _, _) => units +:= new ChunkOutline(p, Color.RED)
        case Vertices(p, _, _, _) => units +:= new ChunkOutline(p, Color.BLUE)
        case Meshable(p, _, _, _, _, _) => units +:= new ChunkOutline(p, Color.GREEN)
      }
    }

    // do memory management
    val nodes = units.par.flatMap(_.resources).seq
    vramGraph ++= nodes
    if (g % 600 == 0) UniExecutor.exec(() => {
      val garbage = vramGraph.garbage(nodes)
      Gdx.app.postRunnable(() => {
        println("deleting: " + garbage)
        garbage.foreach(_.dispose())
        vramGraph --= garbage.toSeq
      })
    })

    // convert to provider
    val provider = new RenderableProvider {
      override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit =
        units.flatMap(_ (interpolation)).foreach(renderables.add)
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
    //val saveFuture = history.last._2.pushToSave()
    vramGraph.managing.foreach(_.dispose())
    //saveFuture.await
    UniExecutor.deactivate()
  }

}
