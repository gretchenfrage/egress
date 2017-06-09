package com.phoenixkahlo.hellcraft

import java.util.UUID

import com.badlogic.gdx.graphics.g3d.RenderableProvider

trait Entity extends RenderableProvider {

  def update(world: World): Unit

}