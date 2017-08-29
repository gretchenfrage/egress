package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator.FreeTypeFontParameter
import com.badlogic.gdx.graphics.g2d.{BitmapFont, SpriteBatch, TextureRegion}
import com.badlogic.gdx.graphics.glutils.ShaderProgram
import com.badlogic.gdx.graphics.{Pixmap, Texture}
import com.badlogic.gdx.scenes.scene2d.ui.Skin

sealed trait SheetTextureID
case object StoneTID extends SheetTextureID
case object SandTID extends SheetTextureID
case object DirtTID extends SheetTextureID
case object BrickTID extends SheetTextureID
case object GrassTID extends SheetTextureID
case object CrosshairTID extends SheetTextureID
case object CursorTID extends SheetTextureID

sealed trait SoloTextureID
case object ButtonTID extends SoloTextureID
case object ButtonActiveTID extends SoloTextureID

sealed trait PixmapID
case object MenuPatchPID extends PixmapID
case object MenuPatchActivePID extends PixmapID

sealed trait FontID
case object HeaderFID extends FontID
case object TitleFID extends FontID
case object ButtonFID extends FontID
case object XFID extends FontID

sealed trait ShaderID
case object TestSID extends ShaderID


trait ResourcePack {

  def sheet: Texture

  def apply(texID: SheetTextureID): TextureRegion

  def sheetRegion(texID: SheetTextureID): TextureRegion = apply(texID)

  def solo(texID: SoloTextureID): Texture

  def font(fontID: FontID): BitmapFont

  def pixmap(pixmapID: PixmapID): Pixmap

  def shader(shaderID: ShaderID): ShaderProgram

}

class DefaultResourcePack extends ResourcePack {

  val sheet = new Texture(Gdx.files.internal("textures.png"))

  val regions: Map[SheetTextureID, TextureRegion] = Seq(
    StoneTID -> 0,
    DirtTID -> 1,
    SandTID -> 2,
    BrickTID -> 3,
    GrassTID -> 4,
    CrosshairTID -> 5,
    CursorTID -> 6
  ) map { case (tid, n) => (tid, new TextureRegion(sheet, (n % 16) * 16, (n - (n % 16)) * 16, 16, 16)) } toMap

  override def apply(texID: SheetTextureID): TextureRegion =
    regions(texID)

  val solos: Map[SoloTextureID, Texture] = Seq(
    ButtonTID -> "button.png",
    ButtonActiveTID -> "button_active.png"
  ) map { case (tid, path) => (tid, new Texture(Gdx.files.internal(path))) } toMap

  override def solo(texID: SoloTextureID): Texture =
    solos(texID)

  val fonts: Map[FontID, BitmapFont] = Seq(
    HeaderFID -> ("raleway.ttf", 75),
    ButtonFID -> ("raleway.ttf", 24),
    TitleFID -> ("raleway.ttf", 32),
    XFID -> ("raleway.ttf", 32)
  ) map {
    case (fid, (path, size)) =>
      val generator = new FreeTypeFontGenerator(Gdx.files.internal(path))
      val parameter = new FreeTypeFontParameter
      parameter.size = size
      val font = generator.generateFont(parameter)
      generator.dispose()
      (fid, font)
  } toMap

  override def font(fontID: FontID): BitmapFont =
    fonts(fontID)

  val pixmaps: Map[PixmapID, Pixmap] = Seq(
    MenuPatchPID -> "button.png",
    MenuPatchActivePID -> "button_active.png"
  ) map { case (pid, path) => (pid, new Pixmap(Gdx.files.internal(path))) } toMap

  override def pixmap(pixmapID: PixmapID): Pixmap =
    pixmaps(pixmapID)

  val shaders: Map[ShaderID, ShaderProgram] = Seq(
    TestSID -> "test"
  ) map { case (sid, path) => sid -> new ShaderProgram(Gdx.files.internal("shaders/" + path + "_v.glsl"),
    Gdx.files.internal("shaders/" + path + "_f.glsl")) } toMap

  override def shader(shaderID: ShaderID): ShaderProgram =
    shaders(shaderID)

}