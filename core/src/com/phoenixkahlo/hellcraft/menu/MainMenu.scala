package com.phoenixkahlo.hellcraft.menu

import com.badlogic.gdx.graphics.GL20
import com.badlogic.gdx.{Gdx, InputAdapter}
import com.badlogic.gdx.scenes.scene2d.Stage
import com.badlogic.gdx.scenes.scene2d.ui._
import com.badlogic.gdx.utils.viewport.ScreenViewport
import com.phoenixkahlo.hellcraft.core.{DefaultTexturePack, HeaderTID, TexturePack}
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState}

class MainMenu extends GameState {

  private var textures: TexturePack = _

  private var stage: Stage = _


  override def onEnter(driver: GameDriver): Unit = {
    textures = new DefaultTexturePack

    stage = new Stage(new ScreenViewport)
    Gdx.input.setInputProcessor(stage)

    val header = new Image(textures.solo(HeaderTID))
    stage.addActor(header)
  }

  override def render(): Unit = {
    Gdx.gl.glClearColor(0.9f, 0.9f, 0.9f, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)
  }

  override def onResize(width: Int, height: Int): Unit = {
    stage.getViewport.update(width, height, true)
  }

  override def onExit(): Unit = {
    Gdx.input.setInputProcessor(new InputAdapter)
  }

}
