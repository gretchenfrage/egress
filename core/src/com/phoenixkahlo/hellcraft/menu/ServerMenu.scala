package com.phoenixkahlo.hellcraft.menu


import java.net.InetSocketAddress

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.scenes.scene2d.{Actor, InputEvent}
import com.badlogic.gdx.scenes.scene2d.ui.Label.LabelStyle
import com.badlogic.gdx.scenes.scene2d.ui.Window.WindowStyle
import com.badlogic.gdx.scenes.scene2d.ui._
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.gamedriver.GameDriver
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.menu.util.{EButton, EButtonStyle, EScroller, EScrollerStyle}
import com.phoenixkahlo.hellcraft.multiplayertest.{ClientServerState, EgressClient}
import com.phoenixkahlo.hellcraft.util.{Cache, HardDriveStringSet}
import other.AppDirs

import scala.collection.{JavaConverters, mutable}
import scala.collection.parallel.mutable.ParSet

class ServerMenu(givenResources: Cache[ResourcePack]) extends AbstractMenu(givenResources) {

  private var servers: java.util.Set[String] = _

  override def onEnter(driver: GameDriver): Unit = {
    servers = HardDriveStringSet(AppDirs.dataDir("egress").resolve("servers.txt"))
    super.onEnter(driver)
  }

  override protected def compile(): Unit = {
    var y: Float = Gdx.graphics.getHeight - 20
    def position(widget: Actor): Unit = {
      widget.setPosition((Gdx.graphics.getWidth - widget.getWidth) / 2, y - widget.getHeight)
      y -= (widget.getHeight + 20)
      stage.addActor(widget)
    }

    val buttonStyle = EButtonStyle(
      resources.pixmap(MenuPatchPID),
      resources.pixmap(MenuPatchActivePID),
      9,
      resources.font(ButtonFID),
      Color.BLACK,
      Color.BLACK,
      Math.min(Math.max(Gdx.graphics.getWidth * 0.4f, 400), Gdx.graphics.getWidth - 40).toInt,
      35
    )

    val header = new Label("SERVERS", new LabelStyle(resources.font(TitleFID), Color.BLACK))
    position(header)

    val vertGroup = new VerticalGroup
    val buttGroup = new ButtonGroup[EButton]
    buttGroup.setMaxCheckCount(1)
    buttGroup.setUncheckLast(true)

    def putButton(text: String): Unit = {
      val button = new EButton(text, buttonStyle)
      toDispose += button
      buttGroup.add(button)

      val container = new Container(button)
      container.pad(10, 5, 0, 5)
      vertGroup.addActor(container)
    }

    putButton("host local server")

    for (server <- JavaConverters.collectionAsScalaIterable(servers))
      putButton(server)

    vertGroup.pack()
    val scrollerStyle = EScrollerStyle(
      resources.pixmap(MenuPatchPID), 9,
      buttonStyle.width + 20 toInt, Gdx.graphics.getHeight - 160
    )
    val scroller = new EScroller(vertGroup, scrollerStyle)
    position(scroller)

    val secondButtonStyle = EButtonStyle(
      resources.pixmap(MenuPatchPID),
      resources.pixmap(MenuPatchActivePID),
      9,
      resources.font(ButtonFID),
      Color.BLACK,
      Color.BLACK,
      buttonStyle.width / 3,
      35
    )

    val joinButton = new EButton("JOIN", secondButtonStyle)
    toDispose += joinButton
    joinButton.setPosition((Gdx.graphics.getWidth - secondButtonStyle.width) / 2 - secondButtonStyle.width - 50,
      y - secondButtonStyle.height)
    stage.addActor(joinButton)
    joinButton.addListener(new ClickListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
        val text = buttGroup.getChecked.getText.toString match {
          case "host local server" => driver.enter(new ClientServerState(new Cache(resources)))
          case str => try {
            val split = str.split(':')
            val address = new InetSocketAddress(split(0), split(1) toInt)
            driver.enter(new EgressClient(address, new Cache(resources)), ifFails = Some(ServerMenu.this))
          } catch {
            case e: Exception => println(e)
          }
        }
      }
    })

    val addButton = new EButton("ADD", secondButtonStyle)
    toDispose += addButton
    addButton.setPosition((Gdx.graphics.getWidth - secondButtonStyle.width) / 2, y - secondButtonStyle.height)
    stage.addActor(addButton)
    addButton.addListener(new ClickListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
        driver.enter(new AddServerMenu(new Cache(resources)))
      }
    })

    val removeButton = new EButton("REMOVE", secondButtonStyle)
    toDispose += removeButton
    removeButton.setPosition((Gdx.graphics.getWidth - secondButtonStyle.width) / 2 + secondButtonStyle.width + 50,
      y - secondButtonStyle.height)
    stage.addActor(removeButton)
    removeButton.addListener(new ClickListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
        servers.remove(buttGroup.getChecked.getText.toString)
        compile()
      }
    })

  }

}
