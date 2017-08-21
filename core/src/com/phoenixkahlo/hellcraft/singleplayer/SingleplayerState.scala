package com.phoenixkahlo.hellcraft.singleplayer

import com.badlogic.gdx.{Gdx, InputAdapter}
import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.{Color, GL20, PerspectiveCamera}
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.{Environment, ModelBatch, Renderable, RenderableProvider}
import com.badlogic.gdx.graphics.g3d.utils.FirstPersonCameraController
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.gamedriver.{Delta, GameDriver, GameState}
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.graphics.`new`.NoInterpolation
import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.oldcore.entity.Avatar
import com.phoenixkahlo.hellcraft.util.DependencyGraph
import com.phoenixkahlo.hellcraft.util.caches.Cache
import com.phoenixkahlo.hellcraft.util.threading.UniExecutor
import other.AppDirs

import scala.concurrent.duration._
import scala.collection.SortedMap


class SingleplayerState(providedResources: Cache[ResourcePack]) extends GameState with Runnable {

  private var save: AsyncSave = _
  private var clock: GametimeClock = _
  private var history: SortedMap[Long, LazyInfWorld] = _
  private var resources: ResourcePack = _
  private var cam: PerspectiveCamera = _
  private var controller: FirstPersonCameraController = _
  private var modelBatch: ModelBatch = _
  private var lights: Environment = _
  private var vramGraph: DependencyGraph = _
  private var updateThread: Thread = _
  private var g = 0

  override def onEnter(driver: GameDriver): Unit = {
    println("activating uni executor")
    UniExecutor.activate(Runtime.getRuntime.availableProcessors() - 2, new Thread(_, "uni exec thread"))

    println("instantiating save")
    val generator = new Generator(32)
    save = new RegionGenAsyncSave(AppDirs.dataDir("egress").resolve("single"), new CarboniteSerialService, generator.genChunk)

    clock = new GametimeClock

    println("instantiating history")
    val world = new LazyInfWorld(save, 0, Map.empty, Map.empty, Set.empty, Set.empty, Set.empty, Map.empty)
        .updateLoaded(LoadDist.neg to LoadDist)
    history = SortedMap(0L -> world)

    println("loading resources")
    resources = providedResources()

    println("creating camera")
    cam = new PerspectiveCamera(90, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.1f
    cam.far = 1000
    cam.position.set(V3F(-10, 10, -10) toGdx)
    cam.lookAt(0, 10, 0)

    println("instantiating controller")
    controller = new FirstPersonCameraController(cam)
    Gdx.input.setInputProcessor(controller)

    println("instantiating model batch")
    modelBatch = new ModelBatch

    println("instantiating lights")
    lights = new Environment
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))

    println("instantiating VRAM graph")
    vramGraph = new DependencyGraph

    Thread.currentThread().setPriority(10)

    println("spawning updating thread")
    updateThread = new Thread(this, "update thread")
    updateThread.setPriority(10)
    clock.reset()
    updateThread.start()

    println("singleplayer state initialized")
  }

  override def run(): Unit = {
    try {
      while (!Thread.interrupted()) {
        // update world
        var world = history.last._2
        world = world.update(Delta.dt.toNanos.toFloat / 1000000000f)
        UniExecutor.point = V3F(cam.position)
        val p = (V3F(cam.position) / 16).floor
        world = world.updateLoaded((p - LoadDist) to (p + LoadDist))

        // manage history
        history = history.updated(world.time, world)
        history = history.rangeImpl(Some(world.time - 5), None)

        // manage time
        if (clock.timeSince(world.time) > (500 milliseconds)) {
          println("can't keep up!")
          clock.forgive(clock.timeSince(world.time) - (500 milliseconds))
        }
        clock.waitUntil(world.time + 1)
      }
    } catch {
      case e: InterruptedException => println("singleplayer shutting down")
    }
  }

  override def render(): Unit = {
    Gdx.graphics.setTitle(Gdx.graphics.getFramesPerSecond.toString)

    // setup
    g += 1

    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)

    // interpolation
    val backRender = 2
    val (toRender, interpolation) = (history.last._2, NoInterpolation)

    // update controller
    controller.update()

    // memory management
    var factories = toRender.renderables(resources)

    val nodes = factories.par.flatMap(_.resources).seq
    vramGraph ++= nodes
    if (g % 600 == 0) UniExecutor.exec(() => {
      val garbage = vramGraph.garbage(nodes)
      Gdx.app.postRunnable(() => {
        garbage.foreach(_.dispose())
        vramGraph --= garbage.toSeq
      })
    })

    // render 3D stuff
    val provider = new RenderableProvider {
      override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit =
        factories.flatMap(_(interpolation)).foreach(renderables.add)
    }
    modelBatch.begin(cam)
    modelBatch.render(provider, lights)
    modelBatch.end()
  }

  override def onExit(): Unit = {
    Gdx.input.setInputProcessor(new InputAdapter)
    updateThread.interrupt()
    updateThread.join()
    val saveFuture = history.last._2.pushToSave()
    vramGraph.managing.foreach(_.dispose())
    saveFuture.await
    UniExecutor.deactivate()
  }

}
