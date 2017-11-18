package com.phoenixkahlo.hellcraft.graphics

trait HUD {

  def components(texturePack: ResourcePack): Seq[HUDComponent]

}

object EmptyHUD extends HUD {
  override def components(texturePack: ResourcePack) = Seq.empty
}