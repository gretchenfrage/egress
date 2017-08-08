package com.phoenixkahlo.hellcraft.infinitetest

import java.io.File
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}
import javax.print.DocFlavor.BYTE_ARRAY

import com.badlogic.gdx.{Gdx, InputAdapter}
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.{Environment, ModelBatch, Renderable, RenderableProvider}
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera, PerspectiveCamera}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.core.entity.Avatar
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.finitetest.SimpleAvatarController
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState, UpdatingGameDriver, UpdatingGameState}
import com.phoenixkahlo.hellcraft.math.{Origin, V3F, V3I}
import com.phoenixkahlo.hellcraft.menu.MainMenu
import com.phoenixkahlo.hellcraft.save.{RegionSave, WorldSave}
import com.phoenixkahlo.hellcraft.util.{BackgroundMeshCompilerExecutor, Cache, DependencyGraph, PriorityExecContext}
import other.AppDirs

import scala.collection.JavaConverters
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

class InfiniteGameState(providedTextures: Cache[ResourcePack]) extends GameState {

  val loadDist = V3I(13, 7, 13)
  val updateDist = V3I(6, 6, 6)
  val renderDist = V3I(12, 4, 12)

  private var lastChunkPos: Option[V3I] = None

  private var deleted: BlockingQueue[ResourceNode] = _
  private var save: WorldSave = _
  private var infinitum: InfinityManager = _
  private var textures: ResourcePack = _
  private var cam: PerspectiveCamera = _
  private var hudCam: OrthographicCamera = _
  private var controller: SimpleAvatarController = _
  private var modelBatch: ModelBatch = _
  private var spriteBatch: SpriteBatch = _
  private var lights: Environment = _
  private var vramGraph: DependencyGraph = _
  private var t = 0
  private var g = 0
  private var updatingThread: Thread = _

  override def onEnter(driver: GameDriver): Unit = {
    deleted = new LinkedBlockingQueue

    val saveFolder = AppDirs.dataDir("egress").resolve("inf").toFile
    saveFolder.mkdir()
    save = new RegionSave(saveFolder.toPath, 8)
    textures = providedTextures()

    println("instantiating world")
    infinitum = new InfinityManager(save, v => {
      if (v.yi < -20) Stone
      else if (v.yi < 0) Dirt
      else if (v.yi == 0) Grass
      else Air
    })
    infinitum.makeLoaded(loadDist.neg until loadDist)

    val avatar = Avatar(pos = V3F(0, 20, 0))
    infinitum.transformChunk(avatar.chunkPos, _.putEntity(avatar))
    println("world instantiated")

    BackgroundMeshCompilerExecutor.setPlayerPos(avatar.pos)

    cam = new PerspectiveCamera(90, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.1f
    cam.far = 1000
    cam.position.set(V3F(-10, 10, -10) toGdx)
    cam.lookAt(0, 10, 0)

    hudCam = new OrthographicCamera(Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    hudCam.setToOrtho(false)

    controller = SimpleAvatarController(cam, avatar.id, () => driver.enter(new MainMenu(new Cache(textures))))
    Gdx.input.setInputProcessor(controller)

    modelBatch = new ModelBatch
    spriteBatch = new SpriteBatch

    lights = new Environment
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))

    vramGraph = DependencyGraph()

    Thread.currentThread().setPriority(Thread.MAX_PRIORITY)

    updatingThread = new Thread(() => {
      var lastRenderTime = System.nanoTime()
      var tickTimeDebt: Duration = Duration.Zero
      while (!Thread.interrupted()) {
        val currRenderTime = System.nanoTime()
        tickTimeDebt += ((currRenderTime - lastRenderTime) nanoseconds)
        val sleepFor = UpdatingGameDriver.dt - tickTimeDebt
        if (sleepFor > Duration.Zero)
          Thread.sleep(sleepFor toMillis)
        while (tickTimeDebt >= UpdatingGameDriver.dt) {
          update()
          tickTimeDebt -= UpdatingGameDriver.dt
        }
        lastRenderTime = currRenderTime
      }
    }, "game updating thread")
    updatingThread.setPriority(Thread.MAX_PRIORITY)
    updatingThread.start()
  }

  override def render(): Unit = {
    // some initial stuff
    g += 1

    val world = infinitum.world

    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)

    // find the camera's chunk position
    val chunkPos = V3F(controller.cam.position) / 16 floor

    // get the renderable factories
    val factories = ((chunkPos - renderDist) to (chunkPos + renderDist)).par.flatMap(world.chunkAt).flatMap(_.renderables(textures, world)).seq

    // do memory management
    val nodes = factories.par.flatMap(_.resources).seq
    vramGraph ++= nodes
    if (g % 600 == 0) {
      while (deleted.size > 0) vramGraph --= Seq(deleted.remove())
      PriorityExecContext(Thread.MIN_PRIORITY).execute(() => {
        val garbage = vramGraph.garbage(nodes)
        println("deleting " + garbage.size + " resources")
        Gdx.app.postRunnable(() => garbage.foreach(_.dispose()))
        deleted.addAll(JavaConverters.asJavaCollection(garbage))
      })
    }

    // render 3D stuff
    val provider = new RenderableProvider {
      override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit =
        factories.flatMap(_()).foreach(renderables.add)
    }
    modelBatch.begin(cam)
    modelBatch.render(provider, lights)
    modelBatch.end()

    // render HUD
    spriteBatch.begin()
    controller.hud.components(textures).foreach(_.draw(spriteBatch))
    spriteBatch.end()
  }

  def update(): Unit = {
    // increment time
    t += 1

    // find the avatar
    val avatar = lastChunkPos.flatMap(infinitum.world.chunkAt(_).flatMap(_.entities.get(controller.avatarID)))
        .getOrElse(infinitum.findEntity(controller.avatarID)).asInstanceOf[Avatar]

    // update the world and controller
    infinitum.update((avatar.chunkPos - updateDist) to (avatar.chunkPos + updateDist), t % 600 == 0)
    infinitum.integrate(controller.update(infinitum.world, lastChunkPos))
    controller.postUpdate(infinitum.world, lastChunkPos)

    // set the loaded chunks
    if (!lastChunkPos.contains(avatar.chunkPos)) {
      infinitum.setLoaded((avatar.chunkPos - loadDist) to (avatar.chunkPos + loadDist))
      lastChunkPos = Some(avatar.chunkPos)
    }

    // update mesh compiler priority
    BackgroundMeshCompilerExecutor.setPlayerPos(avatar.pos)
  }

  override def onExit(): Unit = {
    Gdx.input.setInputProcessor(new InputAdapter)
    updatingThread.interrupt()
    updatingThread.join()
    infinitum.close()
    vramGraph.managing.foreach(_.dispose())
  }

}
