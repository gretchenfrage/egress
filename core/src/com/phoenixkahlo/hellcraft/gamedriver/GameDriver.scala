package com.phoenixkahlo.hellcraft.gamedriver

import com.badlogic.gdx.ApplicationAdapter
import com.phoenixkahlo.hellcraft.core.TexturePack

class GameDriver(startState: GameState) extends ApplicationAdapter {

  private var state: GameState = startState
  private var toEnter: Option[GameState] = None

  def enter(state: GameState): Unit = {
    toEnter = Some(state)
  }

  override def create(): Unit = {
    state.onEnter(this)
  }

  override def render(): Unit = {
    toEnter match {
      case Some(nextState) =>
        state.onExit()
        state = nextState
        state.onEnter(this)
        toEnter = None
      case None =>
    }
    state.render()
  }

  override def resize(width: Int, height: Int): Unit =
    state.onResize(width, height)

  override def dispose(): Unit = {
    state.onExit()
  }

}
