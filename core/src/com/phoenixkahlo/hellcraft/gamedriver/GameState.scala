package com.phoenixkahlo.hellcraft.gamedriver

import com.phoenixkahlo.hellcraft.graphics.ResourcePack

trait GameState {

  def onEnter(driver: GameDriver): Unit

  def render(): Unit

  def onResize(width: Int, height: Int): Unit = {}

  def onExit(): Unit

}
