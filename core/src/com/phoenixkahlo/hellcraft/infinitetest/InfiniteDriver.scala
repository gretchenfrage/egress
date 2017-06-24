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
import com.phoenixkahlo.hellcraft.math.{Origin, Repeated, V3F, V3I}
import com.phoenixkahlo.hellcraft.save.{RegionSave, WorldSave}
import com.phoenixkahlo.hellcraft.util.{DependencyGraph, BackgroundMeshCompilerExecutor, PriorityExecContext}
import other.PerlinNoiseGenerator

import scala.collection.JavaConverters
import scala.collection.immutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.duration._

class InfiniteDriver extends ApplicationAdapter {

  private var deleted: BlockingQueue[RenderableFactory] = _
  private var save: WorldSave = _
  private var world: InfiniteWorld = _
  private var textures: TexturePack = _
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

    val saveFolder = new File("C:\\Users\\kahlo\\Desktop\\inf")
    saveFolder.mkdir()
    save = RegionSave(saveFolder.toPath, 8)

    textures = new DefaultTexturePack

    println("instantiating world")
    world = new InfiniteWorld(save, v =>
      if (v.yi < -20) Stone
      else if (v.yi < 0) Dirt
      else if (v.yi == 0) Grass
      else Air
    )
    world.makeLoaded(V3I(-8, -8, -8) until V3I(8, 8, 8))

    val avatar = Avatar(pos = V3F(0, 20, 0))
    world.transformChunk(avatar.chunkPos, _.putEntity(avatar))
    println("world instantiated")

    BackgroundMeshCompilerExecutor.setPlayerPos(avatar.pos)

    cam = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.1f
    cam.far = 1000
    cam.position.set(V3F(-10, 10, -10) toGdx)
    cam.lookAt(0, 10, 0)

    hudCam = new OrthographicCamera(Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    hudCam.setToOrtho(false)

    controller = SimpleAvatarController(cam, avatar.id)
    Gdx.input.setInputProcessor(controller)

    modelBatch = new ModelBatch
    spriteBatch = new SpriteBatch

    lights = new Environment
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))

    vramGraph = DependencyGraph()
  }

  override def render(): Unit = {
    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)
    t += 1

    // update the world
    world.update()
    world.integrate(controller.update(world))
    controller.postUpdate(world)

    // set the loaded chunks
    val avatar = world.findEntity(controller.avatarID).asInstanceOf[Avatar]
    val pos = avatar.chunkPos
    val r = 4
    val makeLoaded = ((pos - V3I(r, r, r)) to (pos + V3I(r, r, r))).filter(v => v.dist(pos) <= r)
    world.setLoaded(makeLoaded)

    // update chunk compiler priority
    BackgroundMeshCompilerExecutor.setPlayerPos(pos)

    // get the renderable factories
    val factories = world.renderables(textures)
    // do memory management
    vramGraph ++= factories
    if (t % 600 == 0) {
      var accumulator: Seq[RenderableFactory] = Seq[RenderableFactory]()
      while (deleted.size() > 0)
        accumulator = accumulator :+ deleted.take()
      vramGraph --= accumulator

      PriorityExecContext(Thread.MIN_PRIORITY).execute(() => {
        val garbage = vramGraph.garbage(factories)
        println("garbage collecting " + garbage.size + " VRAM resources")
        Gdx.app.postRunnable(() => garbage.foreach(_.dispose()))
        deleted.addAll(JavaConverters.asJavaCollection(garbage))
      })
    }

    val chunkRenderers = vramGraph.managing.filter(_.isInstanceOf[ChunkRenderer]).map(_.asInstanceOf[ChunkRenderer])

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

  override def dispose(): Unit = {
    world.saveAll()
  }

}
