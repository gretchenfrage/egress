package com.phoenixkahlo.hellcraft.graphics

import java.util.Scanner

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Camera, Color}
import com.badlogic.gdx.graphics.g2d.{BitmapFont, GlyphLayout, SpriteBatch, TextureRegion}
import com.badlogic.gdx.utils.Align
import com.phoenixkahlo.hellcraft.math.{Origin2D, V2F, V2I}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

trait HUDComponent {
  def draw(batch: SpriteBatch, cam: Camera): Unit
}

case class ImgHUDComponent(texture: TextureRegion, pos: V2F, size: V2F, color: Color = Color.WHITE) extends HUDComponent {
  override def draw(batch: SpriteBatch, cam: Camera): Unit = {
    val c = batch.getColor
    batch.setColor(color)
    batch.draw(texture, pos.x, pos.y, size.x, size.y)
    batch.setColor(c)
  }
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
      case DownLeft => new StrBoxHUDComponent(str, font, start + V2F(pad, pad), align, size - V2F(pad * 2, pad * 2), color, lineSep, size.y, pad)
      case DownRight => new StrBoxHUDComponent(str, font, start + V2F(-pad, pad), align, size - V2F(pad * 2, pad * 2), color, lineSep, size.y, pad)
      case UpLeft => new StrBoxHUDComponent(str, font, start + V2F(pad, -pad), align, size - V2F(pad * 2, pad * 2), color, lineSep, size.y, pad)
      case UpRight => new StrBoxHUDComponent(str, font, start + V2F(-pad, -pad), align, size - V2F(pad * 2, pad * 2), color, lineSep, size.y, pad)
    }

}

class StrBoxHUDComponent private(text: String, font: BitmapFont, start: V2F, align: Alignment, size: V2F, color: Color, lineSep: Float, maxHeight: Float, pad: Float) extends HUDComponent {
  val lines: Seq[(String, V2F)] = {
    var buffer = new ArrayBuffer[(String, V2F)]
    var words: Seq[(String, String)] = {
      var str = text
      val words = new ArrayBuffer[(String, String)]
      var i = 0
      while (i < str.length) {
        val tokenBuilder = new StringBuilder
        while (i < str.length && !Character.isWhitespace(str(i))) {
          tokenBuilder += str(i)
          str = str.tail
        }
        val delimiterBuilder = new StringBuilder
        while (i < str.length && Character.isWhitespace(str(i))) {
          delimiterBuilder += str(i)
          str = str.tail
        }
        words += (tokenBuilder.toString -> delimiterBuilder.toString)
      }
      def split(token: String): Seq[String] = {
        if (token.size == 0) Seq.empty
        else {
          val layout = new GlyphLayout
          layout.setText(font, token + ' ')
          if (layout.width >= (size.x - 3)) {
            val builder = new StringBuilder
            var i = 0
            do {
              builder.append(token(i))
              i += 1
            } while (i < token.length && {
              layout.setText(font, builder.toString() + ' ')
              layout.width < (size.x - 3)
            })
            builder.deleteCharAt(builder.length - 1)
            builder.toString +: split(token.drop(builder.length))
          } else Seq(token)
        }
      }
      words.flatMap({ case (token, delimiter) => {
        val s: Seq[String] = split(token)
        s.dropRight(1).map(_ -> " ") :+ (s.last -> delimiter)
      }})
    }
    var sumShift: V2F = Origin2D
    var height: Float = 0
    var currStart = start

    var lineHeight: Float = 0

    while (words nonEmpty) {
      var line = ""
      val layout = new GlyphLayout
      var lineDone = false
      while (!(lineDone || words.isEmpty)) {
        val (word, delimiter) = words.head
        val newLine = line + word + delimiter

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
      height += layout.height + lineSep
      lineHeight = layout.height
    }
    if ((height + lineHeight + pad) >= maxHeight) {
      val canFit = ((maxHeight - pad * 2) / (lineHeight + lineSep)).toInt
      val cantFit = buffer.size - canFit
      if (align.isUp) {
        buffer = buffer.take(canFit)
      } else {
        buffer = buffer.takeRight(canFit)
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