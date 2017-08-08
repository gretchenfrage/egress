package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.Gdx
import com.phoenixkahlo.hellcraft.math.V2F
import com.phoenixkahlo.hellcraft.util.ParamCache

class DefaultHUD extends HUD {

  val componentsCache = new ParamCache[ResourcePack,Seq[HUDComponent]](textures => Seq(
    HUDComponent(textures(CrosshairTID), V2F(Gdx.graphics.getWidth / 2 - 15, Gdx.graphics.getHeight / 2 - 15), V2F(30, 30))
  ))

  override def components(texturePack: ResourcePack): Seq[HUDComponent] = componentsCache(texturePack)

}
