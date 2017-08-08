package com.phoenixkahlo.hellcraft.finitetest

import java.io.File
import java.nio.file.{Path, Paths}

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.{Environment, ModelBatch, Renderable, RenderableProvider}
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera, PerspectiveCamera}
import com.badlogic.gdx.math.MathUtils
import com.badlogic.gdx.utils.Pool
import com.badlogic.gdx.{ApplicationAdapter, Gdx}
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Avatar
import com.phoenixkahlo.hellcraft.graphics.{DefaultResourcePack, ResourcePack}
import com.phoenixkahlo.hellcraft.math.{Origin, Up, V3F, V3I}
import com.phoenixkahlo.hellcraft.save.RegionSave
import com.phoenixkahlo.hellcraft.util._
import other.{AppDirs, PerlinNoiseGenerator}

class SimpleDriver extends ApplicationAdapter {

  private var history: Vector[FiniteWorld] = _
  private var world: FiniteWorld = _
  private var texturePack: ResourcePack = _
  private var cam: PerspectiveCamera = _
  private var hudCam: OrthographicCamera = _
  private var controller: SimpleAvatarController = _
  private var modelBatch: ModelBatch = _
  private var spriteBatch: SpriteBatch = _
  private var lights: Environment = _
  private var dependencies: DependencyGraph = _
  private var t = 0

  override def create(): Unit = {
    history = Vector()

    texturePack = new DefaultResourcePack

    world = new FiniteWorld(V3I(16, 16, 16), 16)

    println("loading / generating")

    //val saveFolder = new File("C:\\Users\\kahlo\\Desktop\\save")
    val saveFolder = AppDirs.dataDir("egress").resolve("fin").toFile
    saveFolder.mkdir()
    val save = new RegionSave(saveFolder.toPath, 8)

    MathUtils.random.setSeed("phoenix".hashCode)
    val heights = PerlinNoiseGenerator.generateHeightMap((world.size * 16).xi, (world.size * 16).zi, 0, 128, 9)
    def height(v: V3I): Byte = heights(v.zi * world.size.zi * 16 + v.xi)

    val loaded = save.load(Origin until world.size)
    world = world.mapChunks(chunk => {
      loaded.get(chunk.pos) match {
        case Some(loadedChunk) => loadedChunk
        case None =>
          chunk.mapBlocks(vLocal => {
            val v = vLocal + (chunk.pos * 16)
            val depth = height(v) - v.y
            if (depth > 20) Stone
            else if (depth >= 0) Dirt
            else Air
          })
      }
    })

    /*
    world = world.mapChunks(chunk => {
      save.load(chunk.pos) match {
        case Some(loaded) =>
          println("loading " + chunk.pos)
          loaded
        case None =>
          println("generating" + chunk.pos)
          chunk.mapBlocks(vLocal => {
            val v = vLocal + (chunk.pos * 16)
            val depth = height(v) - v.y
            if (depth > 20) Stone
            else if (depth >= 0) Dirt
            else Air
          })
      }
    })
    */


    /*
    MathUtils.random.setSeed("phoenix".hashCode)
    val heights = PerlinNoiseGenerator.generateHeightMap((world.size * 16).xi, (world.size * 16).zi, 0, 128, 9)
    def height(v: V3I): Byte = heights(v.zi * world.size.zi * 16 + v.xi)
    world = world.mapBlocks(v => {
      val depth = height(v) - v.y
      if (depth > 20) Stone
      else if (depth >= 0) Dirt
      else Air
    })
    world = world.mapBlocks(v => {
      if (world.blockAt(v).get == Dirt && world.blockAt(v + Up).forall(_ == Air)) Grass
      else world.blockAt(v).get
    })
    */
    println("generated")

    /*
    val avatar = new Avatar()
    world = world.transformChunk(avatar.chunkPos, _.putEntity(avatar))
    */
    var avatar: Avatar = null
    world.chunks.flatMap(_.entities.values).find(_.isInstanceOf[Avatar]) match {
      case Some(entity) => avatar = entity.asInstanceOf[Avatar]
      case None =>
        avatar = Avatar()
        world = world.transformChunk(avatar.chunkPos, _.putEntity(avatar))
    }

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

    dependencies = DependencyGraph()
  }

  override def render(): Unit = {
    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)

    BackgroundMeshCompilerExecutor.setPlayerPos(V3F(controller.cam.position))

    // update the world
    t += 1
    if (t % 3 == 0) {
      if (true) {
        world = world.update
        world = world.integrate(controller.update(world))
      } else {
        if (Gdx.input.isKeyPressed(Keys.P)) {
          history = history :+ world
          world = world.update
          world = world.integrate(controller.update(world))
        }
        if (Gdx.input.isKeyPressed(Keys.L) && history.nonEmpty) {
          world = history.last
          history = history.dropRight(1)
        }
        if (history.size > 6000)
          history = history.drop(6000 - history.size)
      }
      controller.postUpdate(world)
    }

    // get the renderable factories
    val factories = world.renderables(texturePack)
    // do memory management
    /*
    dependencies ++= factories
    println("graph size = " + dependencies.managing.size)
    if (t % 600 == 0)
      PriorityExecContext(Thread.MIN_PRIORITY).execute(new Runnable {
        override def run(): Unit = {
          val garbage = dependencies.garbage(factories)
          Gdx.app.postRunnable(new Runnable { override def run(): Unit = garbage.foreach(_.dispose()) })
        }
      })
      */

    // render 3D stuff
    val provider = new RenderableProvider {
      override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit =
        factories.flatMap(_()).foreach(renderables.add)
    }
    modelBatch.begin(cam)
    modelBatch.render(provider, lights)
    modelBatch.end()

    // render HUD
    val hudProvider = new RenderableProvider {
      override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit =
        factories.flatMap(_()).foreach(renderables.add)
    }
    spriteBatch.begin()
    controller.hud.components(texturePack).foreach(_.draw(spriteBatch))
    spriteBatch.end()
  }

  override def dispose(): Unit = {
    val saveFolder = new File("C:\\Users\\kahlo\\Desktop\\save")
    val save = new RegionSave(saveFolder.toPath, 32)

    /*
    (Origin until world.size).par.foreach(v => {
      println("saving " + v)
      save.save(world.chunkAt(v).get)
    })
    */
    save.save((Origin until world.size).map(world.chunkAt(_).get), world)
  }

}
