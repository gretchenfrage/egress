package com.phoenixkahlo.hellcraft.infinitetest


import java.io.File
import java.util.Scanner
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue, LinkedBlockingQueue}

import com.badlogic.gdx.{ApplicationAdapter, Gdx}
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.{Environment, ModelBatch, Renderable, RenderableProvider}
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera, PerspectiveCamera}
import com.badlogic.gdx.math.MathUtils
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.core.entity.Avatar
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.finitetest.SimpleAvatarController
import com.phoenixkahlo.hellcraft.graphics.{ChunkRenderer, DefaultResourcePack, ResourceNode, ResourcePack}
import com.phoenixkahlo.hellcraft.math.{Origin, Repeated, V3F, V3I}
import com.phoenixkahlo.hellcraft.serial.save.{RegionSave, WorldSave}
import com.phoenixkahlo.hellcraft.util.{DependencyGraph, PriorityExecContext, SpatialExecutor}
import other.{AppDirs, PerlinNoiseGenerator}

import scala.collection.JavaConverters
import scala.collection.immutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import scala.collection.JavaConverters._

class InfiniteDriver extends ApplicationAdapter {

  private var deleted: BlockingQueue[ResourceNode] = _
  private var save: WorldSave = _
  private var world: InfinityManager = _
  private var textures: ResourcePack = _
  private var cam: PerspectiveCamera = _
  private var hudCam: OrthographicCamera = _
  private var controller: SimpleAvatarController = _
  private var modelBatch: ModelBatch = _
  private var spriteBatch: SpriteBatch = _
  private var lights: Environment = _
  private var vramGraph: DependencyGraph = _
  private var t = 0

  override def create(): Unit = {
    deleted = new LinkedBlockingQueue

    val saveFolder = AppDirs.dataDir("egress").resolve("inf").toFile
    //val saveFolder = new File("C:\\Users\\Phoenix\\Desktop\\inf")
    saveFolder.mkdir()
    save = new RegionSave(saveFolder.toPath, 8)

    textures = new DefaultResourcePack

    println("instantiating world")
    world = new InfinityManager(save, v => {
      val h = (0 - (Origin.flatten dist v.flatten)).toInt
      if (v.yi < h - 20) Stone
      else if (v.yi < h) Dirt
      else if (v.yi == h) Grass
      else Air
    })
    world.makeLoaded(V3I(-8, -8, -8) until V3I(8, 8, 8))

    val avatar = Avatar(sprinting = false, pos = V3F(0, 20, 0))
    world.transformChunk(avatar.chunkPos, _.putEntity(avatar))
    println("world instantiated")

    //BackgroundMeshCompilerExecutor.setPlayerPos(avatar.pos)
    SpatialExecutor.global.priorityPoint = avatar.pos

    cam = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.1f
    cam.far = 1000
    cam.position.set(V3F(-10, 10, -10) toGdx)
    cam.lookAt(0, 10, 0)

    hudCam = new OrthographicCamera(Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    hudCam.setToOrtho(false)

    controller = SimpleAvatarController(cam, avatar.id, () => Unit)
    Gdx.input.setInputProcessor(controller)

    modelBatch = new ModelBatch
    spriteBatch = new SpriteBatch

    lights = new Environment
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))

    vramGraph = DependencyGraph()
  }

  override def render(): Unit = {
    val times = new ArrayBuffer[Long]
    val log: () => Unit = () => times += System.nanoTime()

    log()

    val load = V3I(8, 8, 8)
    val update = V3I(4, 4, 4)
    val render = V3I(7, 7, 7)

    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)
    t += 1

    log()

    // find the avatar pos
    val avatar = world.findEntity(controller.avatarID).asInstanceOf[Avatar]
    val pos = avatar.chunkPos

    log()

    // update the world
    world.update((pos - update) to (pos + update), t % 120 == 0)
    world.integrate(controller.update(world.world))
    controller.postUpdate(world.world)

    log()

    // set the loaded chunks
    val makeLoaded = (pos - load) to (pos + load)
    world.setLoaded(makeLoaded)

    log()

    // update chunk compiler priority
    //BackgroundMeshCompilerExecutor.setPlayerPos(pos)
    SpatialExecutor.global.priorityPoint = V3F(controller.cam.position)

    log()

    // get the renderable factories
    val factories = world.renderables(textures, (pos - render) to (pos + render))
    // do memory management
    //val nodes = factories.flatMap(_.resources)
    val nodes = world.resources(textures)
    vramGraph ++= nodes
    if (t % 600 == 0) {
      //for (node <- JavaConverters.collectionAsScalaIterable(deleted)) vramGraph --= Seq(node)
      while (deleted.size > 0) vramGraph --= Seq(deleted.remove())
      PriorityExecContext(Thread.MIN_PRIORITY).execute(() => {
        val garbage = vramGraph.garbage(nodes)
        println("deleting " + garbage.size + " resources")
        Gdx.app.postRunnable(() => garbage.foreach(_.dispose()))
        deleted.addAll(JavaConverters.asJavaCollection(garbage))
      })
    }

    log()

    val chunkRenderers = vramGraph.managing.filter(_.isInstanceOf[ChunkRenderer]).map(_.asInstanceOf[ChunkRenderer])

    log()

    // render 3D stuff
    val provider = new RenderableProvider {
      override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit =
        factories.flatMap(_()).foreach(renderables.add)
    }
    modelBatch.begin(cam)
    modelBatch.render(provider, lights)
    modelBatch.end()

    log()

    // render HUD
    spriteBatch.begin()
    controller.hud.components(textures).foreach(_.draw(spriteBatch))
    spriteBatch.end()

    log()

    //println("times = " + times.map(t => (t - times.head) / 1000000))

    print("main loop profile: ")
    for (i <- 0 until (times.size - 1)) {
      print((times(i + 1) - times(i)) / 1000000 + ", ")
    }
    println()
    println("total time = " + (times.last - times.head) / 1000000)
  }

  override def dispose(): Unit = {
    world.close()
  }

}
