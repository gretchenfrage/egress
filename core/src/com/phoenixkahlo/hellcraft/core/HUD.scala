package com.phoenixkahlo.hellcraft.core

trait HUD {

  def components(texturePack: TexturePack): Seq[HUDComponent]

}
