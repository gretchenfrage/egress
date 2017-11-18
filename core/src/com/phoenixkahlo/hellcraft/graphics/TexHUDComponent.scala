package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.Camera
import com.badlogic.gdx.graphics.g2d.{BitmapFont, SpriteBatch, TextureRegion}
import com.phoenixkahlo.hellcraft.math.{V2F, V2I}

trait HUDComponent {
  def draw(batch: SpriteBatch, cam: Camera): Unit
}

case class TexHUDComponent(
                       texture: TextureRegion,
                       pos: V2F,
                       size: V2F
                       ) extends HUDComponent {

  override def draw(batch: SpriteBatch, cam: Camera): Unit =
    batch.draw(texture, pos.x, pos.y, size.x, size.y)

}

case class StrHUDComponent(
                            str: String,
                            font: BitmapFont,
                            pos: V2F
                          ) extends HUDComponent {
  override def draw(batch: SpriteBatch, cam: Camera): Unit =
    font.draw(batch, str, pos.x, pos.y)
}