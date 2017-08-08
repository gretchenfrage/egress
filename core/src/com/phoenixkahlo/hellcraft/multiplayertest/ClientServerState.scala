package com.phoenixkahlo.hellcraft.multiplayertest

import java.net.InetSocketAddress

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Color, GL20}
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.scenes.scene2d.Stage
import com.badlogic.gdx.scenes.scene2d.ui.Label
import com.badlogic.gdx.scenes.scene2d.ui.Label.LabelStyle
import com.phoenixkahlo.hellcraft.core.{ResourcePack, TitleFID}
import com.phoenixkahlo.hellcraft.gamedriver.{GameDriver, GameState}
import com.phoenixkahlo.hellcraft.util.Cache
import other.AppDirs

class ClientServerState(givenResources: Cache[ResourcePack]) extends GameState {

  var driver: GameDriver = _
  var serverInitThread: Thread = _
  var stage: Stage = _
  var server: EgressServer = _
  var client: EgressClient = _

  override def onEnter(driver: GameDriver): Unit = {
    this.driver = driver

    val mul = AppDirs.dataDir("egress").resolve("mul").toFile
    if (mul exists) mul.listFiles.foreach(_.delete())

    stage = new Stage()
    val text = new Label("initializing...", new LabelStyle(givenResources().font(TitleFID), Color.BLACK))
    text.setPosition((Gdx.graphics.getWidth - text.getWidth) / 2, (Gdx.graphics.getHeight - text.getHeight) / 2)
    stage.addActor(text)
    Gdx.input.setInputProcessor(stage)

    serverInitThread = new Thread(() => {
      server = new EgressServer(25565)
      server.start()
      Thread.sleep(5000)
    })
    serverInitThread.start()
  }

  override def render(): Unit = {
    if (serverInitThread isAlive) {
      Gdx.gl.glClearColor(0.9f, 0.9f, 0.9f, 1)
      Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)

      stage.act(Gdx.graphics.getDeltaTime)
      stage.draw()
    } else {
      if (client == null) {
        stage.dispose()
        stage = null
        client = new EgressClient(new InetSocketAddress("localhost", 25565), givenResources)
        client.onEnter(driver)
      }
      client.render()
    }
  }

  override def onExit(): Unit = {
    server.close()
    client.onExit()
  }

}
