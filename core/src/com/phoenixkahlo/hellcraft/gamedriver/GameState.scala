package com.phoenixkahlo.hellcraft.gamedriver

trait GameState {

  def onEnter(): Unit

  def render(): Unit

  def update(): Unit

  def onExit(): Unit

}
