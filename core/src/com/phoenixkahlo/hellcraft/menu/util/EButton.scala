package com.phoenixkahlo.hellcraft.menu.util

import com.badlogic.gdx.graphics.g2d.{Batch, BitmapFont, NinePatch}
import com.badlogic.gdx.graphics.{Color, Pixmap, Texture}
import com.badlogic.gdx.scenes.scene2d.ui.TextButton
import com.badlogic.gdx.scenes.scene2d.ui.TextButton.TextButtonStyle
import com.badlogic.gdx.utils.Disposable

case class EButtonStyle(
                         patch: Pixmap,
                         activePatch: Pixmap,
                         depth: Int,
                         font: BitmapFont,
                         textColor: Color,
                         activeTextColor: Color,
                         width: Int,
                         height: Int,
                         selectable: Boolean = false
                       ) {

  val asTextButtonStyle = new TextButtonStyle
  asTextButtonStyle.checkedFontColor = if (selectable) activeTextColor else textColor
  asTextButtonStyle.checkedOverFontColor = activeTextColor
  asTextButtonStyle.downFontColor = activeTextColor
  asTextButtonStyle.overFontColor = activeTextColor
  asTextButtonStyle.fontColor = textColor
  asTextButtonStyle.font = font

}

class EButton(text: String, style: EButtonStyle)
  extends TextButton(text, style.asTextButtonStyle) with Disposable {

  val patch = new FrameFactory(style.patch, style.depth)(style.width, style.height)
  val activePatch = new FrameFactory(style.activePatch, style.depth)(style.width, style.height)

  setSize(style.width, style.height)

  override def draw(batch: Batch, parentAlpha: Float): Unit = {
    val toDraw =
      if (isPressed || isOver || (style.selectable && isChecked)) activePatch
      else patch
    batch.draw(toDraw, getX, getY, getWidth, getHeight)
    super.draw(batch, parentAlpha)
  }

  override def getPrefHeight: Float = style.height

  override def getPrefWidth: Float = style.width

  override def dispose(): Unit = {
    patch.dispose()
    activePatch.dispose()
  }

}
