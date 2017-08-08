package com.phoenixkahlo.hellcraft.gamedriver

trait UpdatingGameState {

  def onEnter(): Unit

  def render(): Unit

  /**
    * @return whether anything was actually changed
    */
  def update(): Boolean

  def onExit(): Unit

}
