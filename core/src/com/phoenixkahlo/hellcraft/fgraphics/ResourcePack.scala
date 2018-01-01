package com.phoenixkahlo.hellcraft.fgraphics

import java.io.{ByteArrayInputStream, DataInputStream}

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.audio.Sound
import com.badlogic.gdx.files.FileHandle
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics._
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator.FreeTypeFontParameter
import com.badlogic.gdx.graphics.g2d.{BitmapFont, TextureRegion}
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.model.MeshPart
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder
import com.badlogic.gdx.graphics.g3d.{Material, Model}
import com.phoenixkahlo.hellcraft.core.eval.GEval.GEval
import com.phoenixkahlo.hellcraft.core.eval.{Eval, GEval}
import com.phoenixkahlo.hellcraft.math.{V2F, V3F, V4F}
import com.phoenixkahlo.hellcraft.menu.util.FrameFactory
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc

import scala.collection.mutable.ArrayBuffer

sealed trait SheetTextureID extends Serializable
case object StoneTID extends SheetTextureID
case object SandTID extends SheetTextureID
case object DirtTID extends SheetTextureID
case object BrickTID extends SheetTextureID
case object GrassTID extends SheetTextureID
case object CrosshairTID extends SheetTextureID
case object CursorTID extends SheetTextureID
case object SoundTID extends SheetTextureID
case object SunTID extends SheetTextureID
case object MoonTID extends SheetTextureID
case object PhysTID extends SheetTextureID
case object GrayTID extends SheetTextureID
case object StarTID extends SheetTextureID
case object ErrorTID extends SheetTextureID

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
case object ChatFID extends FontID

sealed trait SoundID
case object SnapSID extends SoundID

trait ResourcePack {

  def sheet: Texture

  def apply(texID: SheetTextureID): TextureRegion

  def sheetRegion(texID: SheetTextureID): TextureRegion = apply(texID)

  def solo(texID: SoloTextureID): Texture

  def font(fontID: FontID): BitmapFont

  def pixmap(pixmapID: PixmapID): Pixmap

  def frame(pixmapID: PixmapID, depth: Int, width: Int, height: Int): Texture

  def sound(soundID: SoundID): Sound

}

class DefaultResourcePack extends ResourcePack {

  val sheet = new Texture(Gdx.files.internal("textures.png"))

  val rsize = sheet.getWidth / 16
  val regions: Map[SheetTextureID, TextureRegion] = Seq(
    StoneTID -> 0,
    DirtTID -> 1,
    SandTID -> 2,
    BrickTID -> 3,
    GrassTID -> 4,
    CrosshairTID -> 5,
    CursorTID -> 6,
    SoundTID -> 7,
    SunTID -> 8,
    MoonTID -> 9,
    PhysTID -> 10,
    GrayTID -> 11,
    StarTID -> 12,
    ErrorTID -> 255
  ) map { case (tid, n) => (tid, new TextureRegion(sheet, (n % 16) * rsize, (n - (n % 16)) * rsize, rsize, rsize)) } toMap

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
    XFID -> ("raleway.ttf", 32),
    ChatFID -> ("raleway.ttf", 18)
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

  val sounds: Map[SoundID, Sound] = Seq(
    SnapSID -> "snap.wav"
  ) map { case (sid, path) => (sid, Gdx.audio.newSound(Gdx.files.internal("sounds/" + path))) } toMap

  override def sound(soundID: SoundID): Sound =
    sounds(soundID)

  val _frame = new MemoFunc[(PixmapID, Int, Int, Int), Texture](
    { case (pid, d, w, h) => new FrameFactory(pixmap(pid), d)(w, h) })

  override def frame(pixmapID: PixmapID, depth: Int, width: Int, height: Int): Texture =
    _frame((pixmapID, depth, width, height))

}