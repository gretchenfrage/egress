package com.phoenixkahlo.hellcraft.singleplayer

import com.badlogic.gdx.{Gdx, InputAdapter, InputMultiplexer}
import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.Pixmap.Format
import com.badlogic.gdx.graphics.{Color, GL20, PerspectiveCamera}
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.{DirectionalLight, DirectionalShadowLight}
import com.badlogic.gdx.graphics.g3d._
import com.badlogic.gdx.graphics.g3d.shaders.DefaultShader
import com.badlogic.gdx.graphics.g3d.utils.{BaseShaderProvider, DepthShaderProvider, FirstPersonCameraController, ShaderProvider}
import com.badlogic.gdx.graphics.glutils.FrameBuffer
import com.badlogic.gdx.math.Vector3
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.core.{Densities, Quads, Vertices}
import com.phoenixkahlo.hellcraft.gamedriver.{Delta, GameDriver, GameState}
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.graphics.`new`._
import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.menu.MainMenu
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
  private var infinitum: Infinitum = _
  private var resources: ResourcePack = _
  private var cam: PerspectiveCamera = _
  private var controller: FirstPersonCameraController = _
  private var modelBatch: ModelBatch = _
  private var lightCam: PerspectiveCamera = _
  private var lightBuffer: FrameBuffer = _
  private var lightBatch: ModelBatch = _
  private var depthShader: DepthShader = _
  private var sceneShader: TestShader = _
  private var environment: Environment = _
  private var vramGraph: DependencyGraph = _
  private var updateThread: Thread = _
  private var g = 0

  override def onEnter(driver: GameDriver): Unit = {
    val res = 32

    println("activating uni executor")
    UniExecutor.activate(Runtime.getRuntime.availableProcessors() - 2, new Thread(_, "uni exec thread"), t => {
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

    println("creating camera")
    cam = new PerspectiveCamera(90, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.1f
    cam.far = 1000
    cam.position.set(V3F(-10, 10, -10) toGdx)
    cam.lookAt(0, 10, 0)

    println("instantiating model batch")
    sceneShader = new TestShader(resources.sheet)
    sceneShader.init()
    val lineShader = new LineShader
    lineShader.init()
    modelBatch = new ModelBatch(new ShaderProvider {
      override def getShader(renderable: Renderable): Shader =
        Option(renderable.userData).map(_.asInstanceOf[ShaderID]).getOrElse(DefaultSID) match {
          case CustomSID => sceneShader
          case LineSID => lineShader
          case DefaultSID => Option(renderable.shader).find(_.isInstanceOf[DefaultShader]).getOrElse({
            val shader = new DefaultShader(renderable)
            shader.init()
            renderable.shader = shader
            shader
            })
        }

      override def dispose(): Unit = sceneShader.dispose()
    })

    println("instantiating environment")
    environment = new Environment
    environment.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
    environment.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))

    println("instantiating lighting")
    lightCam = new PerspectiveCamera(120f, 1024, 1024)
    lightCam.near = 0.1f
    lightCam.far = 100f
    lightCam.position.set(0, 10, 0)
    lightCam.lookAt(1, 10, 1)
    lightCam.update()

    lightBuffer = new FrameBuffer(Format.RGBA8888, 1024, 1024, true)
    depthShader = new DepthShader()
    depthShader.init()
    lightBatch = new ModelBatch(new ShaderProvider {
      override def getShader(renderable: Renderable): Shader =
        depthShader

      override def dispose(): Unit =
        depthShader.dispose()
    })

    println("instantiating controller")
    val multiplexer = new InputMultiplexer
    multiplexer.addProcessor(new InputAdapter {
      override def keyDown(keycode: Int): Boolean =
        if (keycode == Keys.ESCAPE) {
          println("closing world")
          driver.enter(new MainMenu(providedResources))
          true
        } else false
    })
    controller = new FirstPersonCameraController(cam)
    multiplexer.addProcessor(controller)
    Gdx.input.setInputProcessor(multiplexer)

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
        UniExecutor.point = V3F(cam.position)
        val p = (V3F(cam.position) / 16).floor
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
    val backRender = 2
    val (toRender, interpolation) = (infinitum(), NoInterpolation)

    // update controller
    controller.update()

    // get render units
    var units = toRender.renderables(resources)

    // add debug units
    if (Gdx.input.isKeyPressed(Keys.ALT_LEFT)) {
      toRender.chunks.values.map(_.terrain).foreach {
        case Densities(p, _) => units +:= new ChunkOutline(p, Color.RED)
        case Vertices(p, _, _) => units +:= new ChunkOutline(p, Color.BLUE)
        case Quads(p, _, _, _) => units +:= new ChunkOutline(p, Color.GREEN)
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

    // render 3D stuff
    val provider = new RenderableProvider {
      override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit =
        units.flatMap(_(interpolation)).foreach(renderables.add)
    }
    lightBuffer.begin()
    Gdx.gl.glClearColor(0, 0, 0, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    lightBatch.begin(lightCam)
    lightBatch.render(provider)
    lightBatch.end()
    lightBuffer.end()

    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)
    modelBatch.begin(cam)
    modelBatch.render(provider, environment)
    modelBatch.end()

  }

  override def onExit(): Unit = {
    Gdx.input.setInputProcessor(new InputAdapter)
    updateThread.interrupt()
    updateThread.join()
    //val saveFuture = history.last._2.pushToSave()
    vramGraph.managing.foreach(_.dispose())
    //saveFuture.await
    UniExecutor.deactivate()
  }

}
