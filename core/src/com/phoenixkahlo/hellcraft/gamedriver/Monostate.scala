package com.phoenixkahlo.hellcraft.gamedriver

trait Monostate {

  def enter(): Unit

  def render(): Unit

  def exit(): Unit

}
