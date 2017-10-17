package com.phoenixkahlo.hellcraft.singleplayer

import com.badlogic.gdx.{Gdx, utils}
import com.badlogic.gdx.graphics._
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.g3d._
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.utils.{RenderableSorter, ShaderProvider}
import com.badlogic.gdx.utils.{Disposable, Pool}
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.graphics.shaders._
import com.phoenixkahlo.hellcraft.math._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class Renderer(resources: ResourcePack) extends Disposable {

  val environment = new Environment
  environment.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
  environment.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))

  val cam = new PerspectiveCamera(90, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
  cam.near = 0.1f
  cam.far = 1000
  //cam.position.set(30, 30, 30)
  //cam.lookAt(0, 25, 0)
  cam.position.set(0, 0, 0)
  cam.lookAt(1, 1, 1)
  cam.up.set(0, 1, 0)

  val terrainShader = new TerrainShader(resources.sheet)
  terrainShader.init()
  val lineShader = new LineShader
  lineShader.init()
  val basicShader = new BasicShader(resources.sheet, TerrainSID)
  basicShader.init()
  val pointShader = new PointShader
  pointShader.init()
  val genericShader = new GenericShader(resources.sheet)
  genericShader.init()
  val particleShader = new ParticleShader(resources.sheet)
  particleShader.init()

  val batch = new ModelBatch(new ShaderProvider {
    override def getShader(renderable: Renderable): Shader =
      renderable.userData match {
        case TerrainSID => terrainShader
        case LineSID => lineShader
        case PointSID => pointShader
        case BasicSID => basicShader
        case GenericSID => genericShader
        case ParticleSID => particleShader
        case _ =>
          println("failed to generate shader for " + renderable + " with userData + " + renderable.userData)
          DontRenderShader
      }

    override def dispose(): Unit = ()
  }, new RenderableSorter {
    override def sort(camera: Camera, renderables: utils.Array[Renderable]): Unit = ()
  })

  val hud = new DefaultHUD
  val spriteBatch = new SpriteBatch()


  def render(world: SWorld, units: Seq[RenderUnit], interpolation: Interpolation): Unit = {
    val skyDistance = LoadDist.fold(Math.max) * 20
    val camPos = V3F(cam.position)

    // set up sunlight
    val cycle = world.time.toFloat / DayCycleTicks.toFloat % 1
    val sunDir = V3F(-Trig.cos(cycle * 360), Trig.sin(cycle * 360), 0)
    val sunPos = camPos + (sunDir * skyDistance)
    val moonPos = camPos + (sunDir.neg * skyDistance)

    val sunMoon = ParticleFactory(resources,
      ParticleType(skyDistance / 8, SunTID) -> sunPos,
      ParticleType(skyDistance / 8, MoonTID) -> moonPos
    )

    // compute sky color
    val dayColor = V3F(0.5089f, 0.6941f, 1f)
    val nightColor = V3F(0.0039f, 0.0471f, 0.1843f)
    val transFrac = 0.04f

    var trans: Float = 0
    var from: V3F = null
    var to: V3F = null
    if (cycle < transFrac / 2 || cycle > 1 - transFrac / 2) {
      // sunrise
      trans = (cycle + transFrac / 2) % 1 / transFrac
      from = nightColor
      to = dayColor
    } else if (cycle > 0.5f - transFrac / 2 && cycle < 0.5f + transFrac / 2) {
      // sunset
      trans = (cycle + transFrac / 2 - 0.5f) / transFrac
      from = dayColor
      to = nightColor
    } else if (cycle < 0.5f) {
      // day
      from = dayColor
      to = dayColor
    } else {
      // night
      from = nightColor
      to = nightColor
    }
    val skyColor = ((to - from) * trans) + from

    // compute sun power
    val fromPow = if (from == dayColor) 1 else -1
    val toPow = if (to == dayColor) 1 else -1
    val lightPow = ((toPow - fromPow) * trans) + fromPow
    if (lightPow > 0) {
      terrainShader.lightPow = lightPow
      genericShader.lightPow = lightPow

      terrainShader.lightPos = sunPos
      genericShader.lightPos = sunPos
    } else {
      val moonPow = 1f / 10f

      terrainShader.lightPow = -lightPow * moonPow
      genericShader.lightPow = -lightPow * moonPow


      terrainShader.lightPos = moonPos
      genericShader.lightPos = moonPos
    }

    // generate stars
    val starAlpha = Trig.clamp(-lightPow, 0, 1)
    val starType: ParticleType = ParticleType(skyDistance / 128, StarTID, new Color(1, 1, 1, starAlpha))
    val starDirs = new ArrayBuffer[V3F]
    val random = new Random(2763548726354L)
    for (_ <- 0 until 1000) {
      val t = random.nextFloat() * 360
      val z = random.nextFloat() * 2 - 1
      starDirs += V3F(
        Math.sqrt(1 - z * z).toFloat * Trig.cos(t),
        Math.sqrt(1 - z * z).toFloat * Trig.sin(t),
        z
      ).rotate(South, cycle * 360)
    }
    val starData = starDirs.map(dir => starType -> (camPos + (dir * skyDistance)))
    val stars = ParticleFactory(resources, starData: _*)

    // clear
    Gdx.gl.glClearColor(skyColor.x, skyColor.y, skyColor.z, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)

    // sort and render
    val (translucent, opaque) = units.partition(_.locationIfTransparent.isDefined)
    val opaqueSeq: Seq[Renderable] = (opaque :+ sunMoon :+ stars).flatMap(_(interpolation)).sortBy(_.userData.hashCode())
    val transSeq = translucent.sortBy(_.locationIfTransparent.get dist camPos * -1).flatMap(_(interpolation))
    val renderSeq = opaqueSeq ++ transSeq

    val provider = new RenderableProvider {
      override def getRenderables(renderables: utils.Array[Renderable], pool: Pool[Renderable]): Unit =
        renderSeq.foreach(renderables.add)
    }

    batch.begin(cam)
    batch.render(provider)
    batch.end()

    /*
    val (translucent, opaque) = units.partition(_.locationIfTransparent.isDefined)
    val renderSeq = ((sunMoon +: opaque) :+ stars) ++
      translucent.sortBy(_.locationIfTransparent.get dist camPos).reverse

    // convert to provider
    /*
    val provider = new RenderableProvider {
      override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit = {
        renderSeq.flatMap(_ (interpolation)).foreach(renderables.add)
      }
    }

    // render scene
    batch.begin(cam)
    batch.render(provider, environment)
    batch.end()
    */
    */

    // render HUD
    spriteBatch.begin()
    for (comp <- hud.components(resources)) {
      comp.draw(spriteBatch)
    }
    spriteBatch.end()

    // dispose of render-local resources
    sunMoon.resources.foreach(_.dispose())
    stars.resources.foreach(_.dispose())
  }

  def onResize(width: Int, height: Int): Unit = {
    cam.viewportWidth = width
    cam.viewportHeight = height
    cam.update()
  }

  override def dispose(): Unit = {
    terrainShader.dispose()
    lineShader.dispose()
    basicShader.dispose()
    genericShader.dispose()
  }

}
