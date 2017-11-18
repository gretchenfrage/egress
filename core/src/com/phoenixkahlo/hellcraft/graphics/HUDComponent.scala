package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Camera, Color}
import com.badlogic.gdx.graphics.g2d.{BitmapFont, GlyphLayout, SpriteBatch, TextureRegion}
import com.badlogic.gdx.utils.Align
import com.phoenixkahlo.hellcraft.math.{Origin2D, V2F, V2I}

import scala.collection.mutable.ArrayBuffer

trait HUDComponent {
  def draw(batch: SpriteBatch, cam: Camera): Unit
}

case class ImgHUDComponent(texture: TextureRegion, pos: V2F, size: V2F) extends HUDComponent {
  override def draw(batch: SpriteBatch, cam: Camera): Unit =
    batch.draw(texture, pos.x, pos.y, size.x, size.y)
}

case class StrHUDComponent(str: String, font: BitmapFont, pos: V2F, color: Color) extends HUDComponent {
  override def draw(batch: SpriteBatch, cam: Camera): Unit = {
    val c = font.getColor
    font.setColor(color)
    font.draw(batch, str, pos.x, pos.y)
    font.setColor(c)
  }
}

sealed trait Alignment {
  def isLeft: Boolean = this == UpLeft || this == DownLeft
  def isRight: Boolean = !isLeft
  def isUp: Boolean = this == UpLeft || this == UpRight
  def isDown: Boolean = !isUp
  def toInt: Int = this match {
    case UpLeft => Align.topLeft
    case UpRight => Align.topRight
    case DownLeft => Align.bottomLeft
    case DownRight => Align.bottomRight
  }
}
case object UpLeft extends Alignment
case object UpRight extends Alignment
case object DownLeft extends Alignment
case object DownRight extends Alignment

object StrBoxHUDComponent {
  def apply(str: String, font: BitmapFont, start: V2F, align: Alignment, size: V2F, color: Color, lineSep: Float, pad: Float): StrBoxHUDComponent =
    align match {
      case DownLeft => StrBoxHUDComponent(str, font, start + V2F(pad, pad), align, size - V2F(pad * 2, pad * 2), color, lineSep)
      case DownRight => StrBoxHUDComponent(str, font, start + V2F(-pad, pad), align, size - V2F(pad * 2, pad * 2), color, lineSep)
      case UpLeft => StrBoxHUDComponent(str, font, start + V2F(pad, -pad), align, size - V2F(pad * 2, pad * 2), color, lineSep)
      case UpRight => StrBoxHUDComponent(str, font, start + V2F(-pad, -pad), align, size - V2F(pad * 2, pad * 2), color, lineSep)
    }

}

case class StrBoxHUDComponent(str: String, font: BitmapFont, start: V2F, align: Alignment, size: V2F, color: Color, lineSep: Float) extends HUDComponent {
  val lines: Seq[(String, V2F)] = {
    val buffer = new ArrayBuffer[(String, V2F)]
    var words = str.split("\\s+")
    var sumShift: V2F = Origin2D
    var currStart = start

    while (words nonEmpty) {
      var line = ""
      val layout = new GlyphLayout
      var lineDone = false
      while (!(lineDone || words.isEmpty)) {
        val word = words.head
        val newLine = line + word + ' '

        layout.setText(font, newLine)

        if (layout.width > size.x) {
          layout.setText(font, line)
          lineDone = true
        } else {
          line = newLine
          words = words.tail
        }
      }
      align match {
        case DownLeft =>
          buffer += ((line, V2F(start.x, currStart.y)))
          currStart -= V2F(0, layout.height + lineSep)
          sumShift += V2F(0, layout.height + lineSep)
        case DownRight =>
          buffer += ((line, V2F(start.x - layout.width, currStart.y)))
          currStart -= V2F(0, layout.height + lineSep)
          sumShift += V2F(0, layout.height + lineSep)
        case UpLeft =>
          buffer += ((line, V2F(start.x, currStart.y)))
          currStart -= V2F(0, layout.height + lineSep)
        case UpRight =>
          buffer += ((line, V2F(start.x - layout.width, currStart.y)))
          currStart -= V2F(0, layout.height + lineSep)
      }
    }

    buffer.map({ case (str, pos) => (str, pos + sumShift) })
  }

  override def draw(batch: SpriteBatch, cam: Camera): Unit = {
    val c = font.getColor
    font.setColor(color)
    for ((str, pos) <- lines) {
      font.draw(batch, str, pos.x, pos.y)
    }
    font.setColor(c)
  }
}