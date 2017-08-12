package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID

import com.badlogic.gdx.{Gdx, InputAdapter}
import com.badlogic.gdx.graphics.{GL20, PerspectiveCamera}
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.{Environment, ModelBatch, Renderable, RenderableProvider}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Avatar
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState}
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}
import com.phoenixkahlo.hellcraft.menu.MainMenu
import com.phoenixkahlo.hellcraft.util.{BackgroundMeshCompilerExecutor, Cache, DependencyGraph, PriorityExecContext}
import other.AppDirs

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
    println("loading")
    /*
    val surface = BlockGrid(v => if (v.yi == 15) Grass else Dirt)
    val chunkGen: V3I => Chunk = p =>
      if (p.yi == -1) new Chunk(p, surface)
      else if (p.yi >= 0) new Chunk(p, BlockGrid.AirGrid)
      else new Chunk(p, BlockGrid.StoneGrid)
    */
    val generator = new Generator

    save = new RegionGenAsyncSave(AppDirs.dataDir("egress").resolve("single"), generator.genChunk)
    println("instantiated save")

    clock = new GametimeClock

    val avatar = Avatar(pos = V3F(0, 32, 0))
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
        world = world.update
        world = world.integrate(controller.mainUpdate(world))
        val avatar = world.findEntity(controller.avatarID).get.asInstanceOf[Avatar]
        BackgroundMeshCompilerExecutor.setPlayerPos(avatar.pos)
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
    val (toRender, interpolation) =
      (history.last._2, history.dropRight(1).lastOption.map(_._2)) match {
        case (ultimate, Some(penultimate)) =>
          (penultimate, Some((ultimate, Math.min(clock.fractionalTicksSince(penultimate.time) - 1, 1))))
        case (ultimate, None) =>
          (ultimate, None)
      }

    // update controller
    controller.camUpdate(toRender, interpolation)

    // memory management
    val factories = toRender.renderables(resources)
    val nodes = factories.par.flatMap(_.resources).seq
    vramGraph ++= nodes
    if (g % 600 == 0) PriorityExecContext(1).execute(() => {
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
    Await.result(saveFuture, Duration.Inf)
  }

}
