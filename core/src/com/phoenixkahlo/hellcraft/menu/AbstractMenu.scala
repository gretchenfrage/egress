package com.phoenixkahlo.hellcraft.menu

import java.awt.Color

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.{Gdx, InputAdapter, InputMultiplexer, InputProcessor}
import com.badlogic.gdx.graphics.GL20
import com.badlogic.gdx.scenes.scene2d.Stage
import com.badlogic.gdx.utils.Disposable
import com.badlogic.gdx.utils.viewport.ScreenViewport
import com.phoenixkahlo.hellcraft.core.ResourcePack
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState}
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}
import com.phoenixkahlo.hellcraft.util.Cache

import scala.collection.mutable.ArrayBuffer

abstract class AbstractMenu(givenResources: Cache[ResourcePack]) extends GameState {

  protected var driver: GameDriver = _
  protected var resources: ResourcePack = _
  protected var stage: Stage = _
  protected val toDispose = new ArrayBuffer[Disposable]

  protected def escapeState: GameState = new MainMenu(new Cache(resources))
  protected val clearColor: V3I = V3I(229, 229, 229)
  protected def processor: InputProcessor = new InputAdapter
  protected def compile(): Unit

  protected def enterMainMenu(): Unit = {
    driver.enter(new MainMenu(new Cache(resources)))
  }

  override def onEnter(driver: GameDriver): Unit = {
    this.driver = driver

    Gdx.input.setCursorCatched(false)

    resources = givenResources()

    stage = new Stage(new ScreenViewport)

    val multiplexer = new InputMultiplexer()

    if (!this.isInstanceOf[MainMenu])
      multiplexer.addProcessor(new InputAdapter {
        override def keyDown(keycode: Int): Boolean =
          if (keycode == Keys.ESCAPE) {
            driver.enter(escapeState)
            true
          } else false
      })
    multiplexer.addProcessor(processor)
    multiplexer.addProcessor(stage)

    Gdx.input.setInputProcessor(multiplexer)

    compile()
  }

  override def render(): Unit = {
    clearColor / 255 match { case V3F(r, g, b) => Gdx.gl.glClearColor(r, g, b, 1) }
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)

    stage.act(Gdx.graphics.getDeltaTime)
    stage.draw()
  }

  override def onResize(width: Int, height: Int): Unit = {
    stage.getViewport.update(width, height, true)
    stage.clear()
    compile()
  }

  override def onExit(): Unit = {
    Gdx.input.setInputProcessor(new InputAdapter)
    toDispose.foreach(_.dispose())
  }

}
