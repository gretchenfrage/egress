package com.phoenixkahlo.hellcraft.prototype

import com.badlogic.gdx.graphics.g3d.RenderableProvider

/**
  * Created by kahlo on 6/10/2017.
  */
trait Entity extends RenderableProvider {

  def update(world: World): Unit

}
