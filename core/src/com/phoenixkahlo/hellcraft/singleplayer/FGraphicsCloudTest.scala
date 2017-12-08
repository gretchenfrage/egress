package com.phoenixkahlo.hellcraft.singleplayer

import java.io.DataInputStream
import java.util.concurrent.ThreadFactory
import java.util.function.Consumer

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.g3d.utils.FirstPersonCameraController
import com.phoenixkahlo.hellcraft.core.request.ExecSeq
import com.phoenixkahlo.hellcraft.fgraphics
import com.phoenixkahlo.hellcraft.fgraphics._
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState}
import com.phoenixkahlo.hellcraft.graphics.DefaultResourcePack
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.collections.spatial.SpatialTemporalQueue
import com.phoenixkahlo.hellcraft.util.threading.UniExecutor

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

class FGraphicsCloudTest extends GameState {
  var renderer: fgraphics.DefaultRenderer = _
  var renders: Seq[Render[_ <: Shader]] = _
  var globals: GlobalRenderData = _
  var processor: FirstPersonCameraController = _

  override def onEnter(driver: GameDriver): Unit = {
    // activate executor
    UniExecutor.activate(0, new ThreadFactory {
      override def newThread(r: Runnable) = new Thread(r)
    }, new Consumer[Throwable] {
      override def accept(t: Throwable) = t.printStackTrace()
    }, SpatialTemporalQueue.equate(1 second, 0))

    // load resources
    val pack = new DefaultResourcePack

    // create the renderer
    renderer = new DefaultRenderer(pack)

    // generate the render monads
    implicit val exec = ExecSeq

    val cloud: GEval[(Seq[BasicTriVert], Seq[Short])] = GEval({
      val vin = new DataInputStream(Gdx.files.internal("clouds/1_verts.dat").read())
      val verts = new ArrayBuffer[BasicTriVert]
      while (vin.available > 0) {
        verts += BasicTriVert(
          V3F(vin.readFloat(), vin.readFloat(), vin.readFloat()),
          V4F.unpackColor(vin.readFloat()),
          V2F(vin.readFloat(), vin.readFloat()),
          V3F(vin.readFloat(), vin.readFloat(), vin.readFloat())
        )
      }

      val iin = new DataInputStream(Gdx.files.internal("clouds/1_indices.dat").read())
      val indices = new ArrayBuffer[Short]
      while (iin.available > 0) {
        indices += iin.readShort()
      }

      println("loaded!")

      (verts, indices)
    })

    val renderable = Renderable[TerrainShader](cloud)
    val render = Render[TerrainShader](renderable, BasicParams(Origin))

    renders = Seq(render)

    // make the globals
    globals = GlobalRenderData(Origin, North, Origin, 1, renderer.asInstanceOf[DefaultRenderer].cam, V4F(1, 1, 1, 1))

    // generate the input processor
    processor = new FirstPersonCameraController(renderer.asInstanceOf[DefaultRenderer].cam)
    Gdx.input.setInputProcessor(processor)
  }

  override def render(): Unit = {
    processor.update()
    globals = globals.copy(camPos = V3F(renderer.cam.position), camDir = V3F(renderer.cam.direction))
    renderer(renders, globals)
  }

  override def onExit(): Unit = {
    renderer.close()
  }
}
