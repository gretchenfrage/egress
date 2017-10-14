package com.phoenixkahlo.hellcraft.singleplayer

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics._
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.g3d._
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.utils.ShaderProvider
import com.badlogic.gdx.utils.{Disposable, Pool}
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.graphics.shaders._
import com.phoenixkahlo.hellcraft.math._


class Renderer(resources: ResourcePack) extends Disposable {

  val environment = new Environment
  environment.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
  environment.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))

  val cam = new PerspectiveCamera(70, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
  cam.near = 0.1f
  cam.far = 1000
  cam.position.set(30, 30, 30)
  cam.lookAt(0, 25, 0)
  cam.up.set(0, 1, 0)

  val terrainShader = new TerrainShader(resources.sheet)
  terrainShader.init()
  val lineShader = new LineShader
  lineShader.init()
  val basicShader = new BasicShader(resources.sheet, TerrainSID)
  basicShader.init()
  val sunShader = new BasicShader(resources.solo(SunTID), null)
  sunShader.init()
  val pointShader = new PointShader
  pointShader.init()
  val genericShader = new GenericShader(resources.sheet)
  genericShader.init()
  val particleShader = new ParticleShader(resources.sheet)
  particleShader.init()

  val sunModel = new SunModel

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
  })

  val hud = new DefaultHUD
  val spriteBatch = new SpriteBatch()

  def render(world: SWorld, units: Seq[RenderUnit], interpolation: Interpolation): Unit = {
    // convert to provider
    val provider = new RenderableProvider {
      override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit = {
        units.flatMap(_(interpolation)).foreach(renderables.add)
      }
    }

    // set up sunlight
    val cycle = world.time.toFloat / DayCycleTicks.toFloat % 1
    val sunDir = V3F(-Trig.cos(cycle * 360), Trig.sin(cycle * 360), 0)
    val sunPos = V3F(cam.position) + (sunDir * LoadDist.fold(Math.max) * 20)

    terrainShader.lightPos = sunPos
    genericShader.lightPos = sunPos

    val dayColor = V3F(0.5089f, 0.6941f, 1f)
    val nightColor = V3F(0.0039f, 0.0471f, 0.1843f)

    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)

    batch.begin(cam)
    batch.render(provider, environment)
    batch.end()

    spriteBatch.begin()
    for (comp <- hud.components(resources)) {
      comp.draw(spriteBatch)
    }
    spriteBatch.end()

    /*
    val toRender: lang.Iterable[RenderableProvider] = JavaConverters.asJavaIterable(providers)
    setupSunlight(world, toRender)

    Gdx.gl.glClearColor(skyColor.x, skyColor.y, skyColor.z, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)

    if (!Gdx.input.isKeyPressed(Keys.M)) {
      batch.begin(cam)
      batch.render(JavaConverters.asJavaIterable(providers), environment)
      batch.render(sunModel, environment, sunShader)
      batch.end()
    } else {
      depthBatch.begin(lightCam)
      depthBatch.render(toRender, environment)
      depthBatch.end()
    }

    spriteBatch.begin()
    for (comp <- hud.components(resources)) {
      comp.draw(spriteBatch)
    }
    spriteBatch.end()

    g += 1
     */
  }

  /*
  def setupSunlight(world: SWorld, toRender: lang.Iterable[RenderableProvider]): Unit = {
    if (g % 1 != 0)
      return

    val cycle = world.time.toFloat / DayCycleTicks.toFloat % 1
    val rotation = 15
    val sunDir = V3F(-Trig.cos(cycle * 360), Trig.sin(cycle * 360), 0)
    val worldCenter = (V3F(cam.position) / 32 floor) * 32
    val sunPos = (worldCenter + (sunDir * worldBoxRad * 1.5f)).rotate(Up, rotation)
    //val sunPos = V3F(worldBoxRad, worldBoxRad * 0.5f, 0)

    lightCam.position.set(sunPos toGdx)
    lightCam.up.set(Up toGdx)
    //lightCam.lookAt(worldCenter.toGdx)
    lightCam.direction.set(sunDir.neg.toGdx)
    lightCam.update()

    //skyColor = V3F(0.5089f, 0.6941f, 1f)

    val dayColor = V3F(0.5089f, 0.6941f, 1f)
    val nightColor = V3F(0.0039f, 0.0471f, 0.1843f)
    val transFrac = 0.1f

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

    skyColor = ((to - from) * trans) + from

    val fromPow = if (from == dayColor) 1 else -1
    val toPow = if (to == dayColor) 1 else -1
    val lightPow = Math.max(((toPow - fromPow) * trans) + fromPow, 0)
    terrainShader.lightPow = lightPow
    genericShader.lightPow = lightPow

    //terrainShader.lightPow = 1

    val sunDist = cam.far * 0.9f
    val scale = 50
    sunModel.renderable.worldTransform.set(Array[Float](
      1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1
    ))
    sunModel.renderable.worldTransform.rotate(Up toGdx, rotation)
    sunModel.renderable.worldTransform
      .translate(((East * sunDist * Trig.cos(cycle * 360)) + (Up * sunDist * Trig.sin(cycle * 360))) toGdx)
    sunModel.renderable.worldTransform.rotate(South toGdx, cycle * 360)
    sunModel.renderable.worldTransform.rotate(Down toGdx, 90)
    sunModel.renderable.worldTransform.scale(scale, scale, scale)

    lightBuffer.begin()

    Gdx.gl.glClearColor(1, 1, 1, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)

    depthBatch.begin(lightCam)
    depthBatch.render(toRender, environment)
    depthBatch.end()

    lightBuffer.end()

    terrainShader.depthMap = lightBuffer.getColorBufferTexture
    genericShader.depthMap = lightBuffer.getColorBufferTexture

  }

  def onResize(width: Int, height: Int): Unit = {
    cam.viewportWidth = width
    cam.viewportHeight = height
    cam.update()
  }

  def render(world: SWorld, providers: Seq[RenderableProvider]): Unit = {
    val toRender: lang.Iterable[RenderableProvider] = JavaConverters.asJavaIterable(providers)
    setupSunlight(world, toRender)

    Gdx.gl.glClearColor(skyColor.x, skyColor.y, skyColor.z, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)

    if (!Gdx.input.isKeyPressed(Keys.M)) {
      batch.begin(cam)
      batch.render(JavaConverters.asJavaIterable(providers), environment)
      batch.render(sunModel, environment, sunShader)
      batch.end()
    } else {
      depthBatch.begin(lightCam)
      depthBatch.render(toRender, environment)
      depthBatch.end()
    }

    spriteBatch.begin()
    for (comp <- hud.components(resources)) {
      comp.draw(spriteBatch)
    }
    spriteBatch.end()

    g += 1
  }
  */

  def onResize(width: Int, height: Int): Unit = {
    cam.viewportWidth = width
    cam.viewportHeight = height
    cam.update()
  }

  override def dispose(): Unit = {
    terrainShader.dispose()
    lineShader.dispose()
    basicShader.dispose()
    sunShader.dispose()
    sunModel.dispose()
    genericShader.dispose()
  }

}
