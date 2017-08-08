package com.phoenixkahlo.hellcraft.core

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.Texture
import com.badlogic.gdx.graphics.g2d.TextureRegion

sealed trait TextureID

case object StoneTID extends TextureID
case object SandTID extends TextureID
case object DirtTID extends TextureID
case object BrickTID extends TextureID
case object GrassTID extends TextureID

case object CrosshairTID extends TextureID

sealed trait SoloTextureID

case object HeaderTID extends SoloTextureID

trait TexturePack {
  def apply(texID: TextureID): TextureRegion

  def solo(texID: SoloTextureID): Texture

  def sheet: Texture
}

class DefaultTexturePack extends TexturePack {

  val sheet = new Texture(Gdx.files.internal("textures.png"))

  val regions: Map[TextureID, TextureRegion] = Seq(
    StoneTID -> 0,
    DirtTID -> 1,
    SandTID -> 2,
    BrickTID -> 3,
    GrassTID -> 4,
    CrosshairTID -> 5
  ) map { case (tid, n) => (tid, new TextureRegion(sheet, (n % 16) * 16, (n - (n % 16)) * 16, 16, 16)) } toMap

  override def apply(texID: TextureID): TextureRegion =
    regions(texID)

  val solos: Map[SoloTextureID, Texture] = Seq(
    HeaderTID -> "header.png"
  ) map { case (tid, path) => (tid, new Texture(Gdx.files.internal(path))) } toMap

  override def solo(texID: SoloTextureID): Texture =
    solos(texID)
}