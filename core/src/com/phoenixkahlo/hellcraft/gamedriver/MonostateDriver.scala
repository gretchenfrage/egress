package com.phoenixkahlo.hellcraft.gamedriver

import com.badlogic.gdx.ApplicationAdapter

class MonostateDriver(state: Monostate) extends ApplicationAdapter {

  override def create(): Unit = {
    state.enter()
  }

  override def render(): Unit = {
    state.render()
  }

  override def dispose(): Unit = {
    state.exit()
  }

}
