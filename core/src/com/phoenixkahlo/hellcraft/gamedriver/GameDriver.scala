package com.phoenixkahlo.hellcraft.gamedriver

import com.badlogic.gdx.ApplicationAdapter
import com.phoenixkahlo.hellcraft.core.ResourcePack

class GameDriver(startState: GameState) extends ApplicationAdapter {

  private var state: GameState = startState
  private var toEnter: Option[GameState] = None
  private var ifFails: Option[GameState] = None

  def enter(state: GameState, ifFails: Option[GameState] = None): Unit = {
    toEnter = Some(state)
    this.ifFails = ifFails
  }

  override def create(): Unit = {
    state.onEnter(this)
  }

  override def render(): Unit = {
    toEnter match {
      case Some(nextState) =>
        state.onExit()
        try {
          state = nextState
          state.onEnter(this)
          toEnter = None
          ifFails = None
        } catch {
          case e: Exception => ifFails match {
            case Some(failSafe) =>
              println("caught and recovering from:")
              e.printStackTrace()
              state = failSafe
              state.onEnter(this)
              toEnter = None
              ifFails = None
            case None => throw e
          }
        }
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
