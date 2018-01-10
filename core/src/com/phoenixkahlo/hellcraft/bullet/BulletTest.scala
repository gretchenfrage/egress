package com.phoenixkahlo.hellcraft.bullet

import com.badlogic.gdx.{Gdx, InputProcessor}
import com.badlogic.gdx.graphics.g3d.utils.FirstPersonCameraController
import com.phoenixkahlo.hellcraft.core.Blocks.Brick
import com.phoenixkahlo.hellcraft.core.graphics.{FreeCube, FreeCubeParams}
import com.phoenixkahlo.hellcraft.fgraphics.{BrickTID, DefaultRenderer, DefaultResourcePack, GenericShader, GlobalRenderData, Offset, Render, Renderer, ResourcePack, Shader}
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState}
import com.phoenixkahlo.hellcraft.math.{Ones, Origin, V3F, V4I}
import com.phoenixkahlo.hellcraft.util.collections.spatial.SpatialTemporalQueue
import com.phoenixkahlo.hellcraft.util.threading.UniExecutor

class BulletTest extends GameState {
  private var pack: ResourcePack = _
  private var renderer: Renderer = _
  private var controller: FirstPersonCameraController = _

  override def onEnter(driver: GameDriver): Unit = {
    UniExecutor.activate(0, new Thread(_), _.printStackTrace(), SpatialTemporalQueue.timeDoesntMatter, Ones)

    pack = new DefaultResourcePack

    renderer = new DefaultRenderer(pack)

    controller = new FirstPersonCameraController(renderer.cam)
    Gdx.input.setInputProcessor(controller)
  }

  override def render(): Unit = {
    controller.update()

    val globals = GlobalRenderData(
      V3F(renderer.cam.position), V3F(renderer.cam.direction),
      Origin, 1,
      V4I.ones, 90
    )

    val renders: Seq[Render[_ <: Shader]] = Seq(
      Render[GenericShader](FreeCube(FreeCubeParams(BrickTID, V4I.ones)), Offset.default)
    )
    renderer(renders, globals)
  }

  override def onExit(): Unit = ()
}
