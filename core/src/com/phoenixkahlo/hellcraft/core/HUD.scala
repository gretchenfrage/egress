package com.phoenixkahlo.hellcraft.core

trait HUD {

  def components(texturePack: ResourcePack): Seq[HUDComponent]

}
