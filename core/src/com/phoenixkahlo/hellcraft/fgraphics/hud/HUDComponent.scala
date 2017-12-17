package com.phoenixkahlo.hellcraft.fgraphics.hud

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
    //println("text = " + text)
    // tokenize the text into tokens and delimiters
    var parts: Seq[Either[String, String]] = {
      val parts = new ArrayBuffer[Either[String, String]]
      var i = 0
      while (i < text.length) {
        val token = new StringBuilder
        while (i < text.length && !Character.isWhitespace(text(i))) {
          token += text(i)
          i += 1
        }
        if (token nonEmpty) {
          //println("(token, i) = " + (token, i))
          parts += Left(token.toString())
        }

        val delimiter = new StringBuilder
        while (i < text.length && Character.isWhitespace(text(i))) {
          delimiter += text(i)
          i += 1
        }
        if (delimiter nonEmpty) {
          //println("(delimiter, i) = " + (delimiter, i))
          parts += Right(delimiter.toString)
        }
      }
      parts
    }
    //println("parts = " + parts)
    // helper functions
    // also, record the line height
    var lineHeight: Float = Float.NaN
    val layout = new GlyphLayout
    def strSize(str: String): V2F = {
      layout.setText(font, str)
      lineHeight = layout.height
      V2F(layout.width, layout.height)
    }
    def fits(str: String): Boolean = strSize(str).x <= size.x
    // build the lines
    val lines = new ArrayBuffer[String]
    while ({
      // always drop leading delimiters before possibly iterating
      parts = parts.dropWhile(_ isRight)
      parts nonEmpty
    }) {
      // this loop shall build a single line
      // this buffer holds iterative possibilities for the current line and remaining parts
      val iterations = new ArrayBuffer[(String, Seq[Either[String, String]])]
      iterations += ("" -> parts)
      // build it until it's too big
      do {
        iterations.last._2.head match {
          // a token that can entirely fit
          case Left(token) if fits(token) =>
            iterations += ((iterations.last._1 + token) -> iterations.last._2.tail)
          // a token that has to be split
          case Left(token) =>
            // each character in the token is a possibility
            val tail = iterations.last._2.tail
            for ((char, i) <- token.zipWithIndex) {
              iterations += ((iterations.last._1 + char) -> (Left(token.drop(i)) +: tail))
            }
          // a delimiter
          case Right(delimiter) =>
            iterations += ((iterations.last._1 + delimiter) -> iterations.last._2.tail)
        }
      } while (fits(iterations.last._1) && iterations.last._2.nonEmpty && {
        val nextIsNewline = iterations.last._2.head match {
          case Right(str) if str contains '\n' => true
          case _ => false
        }
        !nextIsNewline
      })
      //println("iterations = " + iterations)
      // search for the last possibility that worked
      val (line, newParts) = iterations.reverse.find({ case (str, _) => fits(str) }).get
      // append and replace
      lines += line
      parts = newParts
    }
    //println("lines = " + lines)
    // position them
    var p = start
    var shift: V2F = Origin2D
    var positioned: Seq[(String, V2F)] = lines.map(line => align match {
      case DownLeft =>
        try line -> p
        finally {
          p -= V2F(0, layout.height + lineSep)
          shift += V2F(0, layout.height + lineSep)
        }
      case DownRight =>
        try line -> (p - V2F(layout.width, 0))
        finally {
          p -= V2F(0, layout.height + lineSep)
          shift += V2F(0, layout.height + lineSep)
        }
      case UpLeft =>
        try line -> p
        finally p -= V2F(0, layout.height + lineSep)
      case UpRight =>
        try line -> p
        finally p -= V2F(0, layout.height + lineSep)
    })
    positioned = positioned.map({ case (str, vec) => (str, vec + shift) })
    //println("positioned = " + positioned)
    // truncate
    if (((lines.size + 1) * (lineHeight + pad)) >= maxHeight) {
      val canFit = ((maxHeight - pad * 2) / (lineHeight + lineSep)).toInt
      if (align.isUp) {
        positioned = positioned.take(canFit)
      } else {
        positioned = positioned.takeRight(canFit)
      }
    }
    //println("truncated = " + positioned)
    // return
    positioned
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