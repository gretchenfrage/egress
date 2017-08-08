package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.graphics.g2d.{SpriteBatch, TextureRegion}
import com.phoenixkahlo.hellcraft.math.V2F

case class HUDComponent(
                       texture: TextureRegion,
                       pos: V2F,
                       size: V2F
                       ) {

  def draw(batch: SpriteBatch): Unit =
    batch.draw(texture, pos.x, pos.y, size.x, size.y)

}