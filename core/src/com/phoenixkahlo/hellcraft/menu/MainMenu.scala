package com.phoenixkahlo.hellcraft.menu

import java.awt.Desktop

import com.badlogic.gdx.graphics.{Color, GL20}
import com.badlogic.gdx.scenes.scene2d.{Actor, InputEvent, Stage}
import com.badlogic.gdx.scenes.scene2d.ui.Button.ButtonStyle
import com.badlogic.gdx.scenes.scene2d.ui.Label.LabelStyle
import com.badlogic.gdx.scenes.scene2d.ui.TextButton.TextButtonStyle
import com.badlogic.gdx.scenes.scene2d.ui._
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener
import com.badlogic.gdx.utils.Align
import com.badlogic.gdx.utils.viewport.ScreenViewport
import com.badlogic.gdx.{Gdx, InputAdapter}
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.finitetest.FiniteGameState
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState}
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.infinitetest.InfiniteGameState
import com.phoenixkahlo.hellcraft.menu.util.{EButton, EButtonStyle}
import com.phoenixkahlo.hellcraft.singleplayer.SingleplayerState
import com.phoenixkahlo.hellcraft.util.Cache
import other.AppDirs

class MainMenu(providedResources: Cache[ResourcePack]) extends AbstractMenu(providedResources) {

  def this() =
    this(new Cache(new DefaultResourcePack))

  override protected def compile(): Unit = {
    addBackButton(() => Gdx.app.exit())

    var y: Float = Gdx.graphics.getHeight - 70
    def position(widget: Actor): Unit = {
      widget.setPosition((Gdx.graphics.getWidth - widget.getWidth) / 2, y - widget.getHeight)
      y -= (widget.getHeight + 20)
      stage.addActor(widget)
    }

    val header = new Label("EGRESS", new LabelStyle(resources.font(HeaderFID), new Color(0x22313FFF)))
    position(header)

    y -= 50

    val buttonStyle = EButtonStyle(
      resources.pixmap(MenuPatchPID),
      resources.pixmap(MenuPatchActivePID),
      9,
      resources.font(ButtonFID),
      Color.BLACK,
      Color.BLACK,
      header.getWidth toInt,
      35
    )

    val singleplayerButton = new EButton("singleplayer", buttonStyle)
    singleplayerButton.addListener(new ClickListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
        driver.enter(new SingleplayerState(providedResources))
      }
    })
    position(singleplayerButton)
    toDispose += singleplayerButton

    /*
    val finiteButton = new EButton("finite", buttonStyle)
    finiteButton.addListener(new ClickListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
        driver.enter(new FiniteGameState(new Cache(resources)))
      }
    })
    position(finiteButton)
    toDispose += finiteButton

    val infiniteButton = new EButton("infinite", buttonStyle)
    infiniteButton.addListener(new ClickListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
        driver.enter(new InfiniteGameState(new Cache(resources)))
      }
    })
    position(infiniteButton)
    toDispose += infiniteButton

    val multiplayerButton = new EButton("multiplayer", buttonStyle)
    multiplayerButton.addListener(new ClickListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
        driver.enter(new ServerMenu(new Cache(resources)))
      }
    })
    position(multiplayerButton)
    toDispose += multiplayerButton
    */

    val openDirButton = new EButton("open directory", buttonStyle)
    openDirButton.addListener(new ClickListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
        Desktop.getDesktop.open(AppDirs.dataDir("egress").toFile)
      }
    })
    position(openDirButton)
    toDispose += openDirButton
  }

}