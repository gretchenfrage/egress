package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.{Gdx, InputAdapter}
import com.badlogic.gdx.graphics.{Color, GL20, PerspectiveCamera}
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.{Environment, ModelBatch, Renderable, RenderableProvider}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Avatar
import com.phoenixkahlo.hellcraft.gamedriver.{Delta, GameDriver, GameState}
import com.phoenixkahlo.hellcraft.graphics.{ChunkOutlineRenderer, ResourcePack}
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}
import com.phoenixkahlo.hellcraft.menu.MainMenu
import com.phoenixkahlo.hellcraft.threading.UniExecutor
import com.phoenixkahlo.hellcraft.util._
import other.AppDirs

import scala.collection.mutable.ArrayBuffer
import scala.collection.{SortedMap, SortedSet}
import scala.concurrent.Await
import scala.concurrent.duration._

class SingleplayerState(providedResources: Cache[ResourcePack]) extends GameState with Runnable {

  private var save: AsyncSave = _
  private var clock: GametimeClock = _
  private var history: SortedMap[Long, LazyInfWorld] = _
  private var resources: ResourcePack = _
  private var cam: PerspectiveCamera = _
  private var controller: SingleplayerController = _
  private var modelBatch: ModelBatch = _
  private var lights: Environment = _
  private var vramGraph: DependencyGraph = _
  private var updateThread: Thread = _
  private var g = 0

  override def onEnter(driver: GameDriver): Unit = {
    println("activating uni executor")
    UniExecutor.activate(Runtime.getRuntime.availableProcessors() - 2, new Thread(_, "uni exec thread"))

    println("loading")
    val generator = new Generator

    save = new RegionGenAsyncSave(AppDirs.dataDir("egress").resolve("single"), new CarboniteSerialService,
      generator.genChunk)
    println("instantiated save")

    clock = new GametimeClock

    val avatar = Avatar(pos = V3F(0.5f, generator.heightsAt(0, 0).await(0, 0).toInt, 0.5f))
    val world = new LazyInfWorld(save, 0, Map.empty, Map.empty, Set.empty, Set.empty, Set.empty, Map.empty)
      .updateLoaded(Seq(avatar.chunkPos))
      .integrate(Seq(AddEntity(avatar, UUID.randomUUID())))
      .updateLoaded(LoadDist.neg to LoadDist)
    history = SortedMap(0L -> world)
    println("instantiated history")

    resources = providedResources()
    println("loaded resources")

    cam = new PerspectiveCamera(90, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.1f
    cam.far = 1000
    cam.position.set(V3F(-10, 10, -10) toGdx)
    cam.lookAt(0, 10, 0)
    println("created camera")

    controller = new SingleplayerController(cam, avatar.id, () => driver.enter(new MainMenu(providedResources)))
    Gdx.input.setInputProcessor(controller)
    println("instantiated controller")

    modelBatch = new ModelBatch
    println("instantiated model batch")

    lights = new Environment
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))
    println("instantiated lights")

    vramGraph = DependencyGraph()
    println("instantiated VRAM graph")

    Thread.currentThread().setPriority(Thread.MAX_PRIORITY)

    updateThread = new Thread(this, "update thread")
    updateThread.setPriority(10)
    clock.reset()
    updateThread.start()
    println("spawned updating thread")
  }

  override def run(): Unit = {
    try {
      while (!Thread.interrupted()) {
        // update world
        var world = history.last._2
        world = world.update(Delta.dt.toNanos.toFloat / 1000000000f)
        world = world.integrate(controller.mainUpdate(world))
        val avatar = world.findEntity(controller.avatarID).get.asInstanceOf[Avatar]
        UniExecutor.point = avatar.pos
        world = world.updateLoaded((avatar.chunkPos - LoadDist) to (avatar.chunkPos + LoadDist))

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
    val (toRender, interpolation) =
      (history.dropRight(backRender - 1).lastOption.map(_._2), history.dropRight(backRender).lastOption.map(_._2)) match {
        case (Some(ultimate), Some(penultimate)) =>
          (penultimate, Some((ultimate, Math.min(clock.fractionalTicksSince(penultimate.time) - backRender, 1))))
        case _ => (history.last._2, None)
      }

    // update controller
    controller.camUpdate(toRender, interpolation)

    // memory management
    var factories = toRender.renderables(resources)

    val nodes = factories.par.flatMap(_.resources).seq
    vramGraph ++= nodes
    if (g % 600 == 0) PriorityExecContext(1).execute(() => {
      val garbage = vramGraph.garbage(nodes)
      Gdx.app.postRunnable(() => {
        garbage.foreach(_.dispose())
        vramGraph --= garbage.toSeq
      })
    })

    // add debug renderables
    if (Gdx.input.isKeyPressed(Keys.F)) {
      factories ++= toRender.border.toSeq.map(new ChunkOutlineRenderer(_, Color.YELLOW))
      factories ++= toRender.futures.keySet.toSeq.map(new ChunkOutlineRenderer(_, Color.PURPLE))
    } else if (Gdx.input.isKeyPressed(Keys.R)) {
      val (complete, notComplete) = toRender.futures.partition(_._2.query.isDefined)
      factories ++= complete.keySet.toSeq.map(new ChunkOutlineRenderer(_, Color.YELLOW))
      factories ++= notComplete.keySet.toSeq.map(new ChunkOutlineRenderer(_, Color.PURPLE))
    }

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
    UniExecutor.deactivate()
    val saveFuture = history.last._2.pushToSave()
    vramGraph.managing.foreach(_.dispose())
    saveFuture.await
  }

}
