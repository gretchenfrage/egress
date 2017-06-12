package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.Texture
import com.badlogic.gdx.graphics.g2d.TextureRegion

import scala.collection.immutable.HashMap

sealed trait TextureID

case object StoneTID extends TextureID
case object SandTID extends TextureID
case object DirtTID extends TextureID

trait TexturePack {
  def apply(texID: TextureID): TextureRegion

  def texture: Texture
}

class DefaultTexturePack extends TexturePack {

  val texture = new Texture(Gdx.files.internal("textures.png"))

  val regions: Map[TextureID, TextureRegion] = Seq(
    StoneTID -> 0,
    DirtTID -> 1,
    SandTID -> 2
  ) map { case (tid, n) => (tid, new TextureRegion(texture, (n % 16) * 16, (n - (n % 16)) * 16, 16, 16)) } toMap

  override def apply(texID: TextureID) = regions(texID)

}