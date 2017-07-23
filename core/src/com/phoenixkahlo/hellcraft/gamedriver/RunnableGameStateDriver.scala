package com.phoenixkahlo.hellcraft.gamedriver

import com.badlogic.gdx.ApplicationAdapter

class RunnableGameStateDriver(state: RunnableGameState) extends ApplicationAdapter {

  override def create(): Unit = {
    state.onEnter()
    new Thread(state).start()
  }

  override def render(): Unit = {
    state.render()
  }

  override def dispose(): Unit = {
    state.onExit()
  }

}
