package com.phoenixkahlo.hellcraft.graphics

trait HUD {

  def components(texturePack: ResourcePack): Seq[HUDComponent]

}
