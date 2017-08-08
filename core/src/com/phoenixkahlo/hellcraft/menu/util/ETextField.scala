package com.phoenixkahlo.hellcraft.menu.util

import com.badlogic.gdx.graphics.g2d.{Batch, BitmapFont, Sprite, TextureRegion}
import com.badlogic.gdx.graphics.{Color, Pixmap}
import com.badlogic.gdx.scenes.scene2d.ui.TextField
import com.badlogic.gdx.scenes.scene2d.ui.TextField.TextFieldStyle
import com.badlogic.gdx.scenes.scene2d.utils.SpriteDrawable
import com.badlogic.gdx.utils.Disposable
import com.phoenixkahlo.hellcraft.math.V2F

case class ETextFieldStyle(
                          patch: Pixmap,
                          depth: Int,
                          font: BitmapFont,
                          textColor: Color,
                          cursor: TextureRegion,
                          width: Int,
                          height: Int,
                          overshoot: V2F = V2F(0, 0)
                          ) {

  val asTextFieldStyle = new TextFieldStyle
  asTextFieldStyle.font = font
  asTextFieldStyle.fontColor = textColor
  asTextFieldStyle.cursor = {
    val sprite = new Sprite(cursor)
    new SpriteDrawable(sprite)
  }

}

class ETextField(style: ETextFieldStyle) extends TextField("", style.asTextFieldStyle) with Disposable {

  val patch = new FrameFactory(style.patch, style.depth)(style.width, style.height)

  setSize(style.width + style.overshoot.x * 2, style.height + style.overshoot.y * 2)

  override def getPrefWidth: Float = style.width + style.overshoot.x * 2

  override def getPrefHeight: Float = style.height + style.overshoot.y * 2

  override def draw(batch: Batch, parentAlpha: Float): Unit = {
    batch.draw(
      patch,
      getX - style.overshoot.x, getY - style.overshoot.y,
      getWidth + style.overshoot.x * 2, getHeight + style.overshoot.y * 2
    )
    super.draw(batch, parentAlpha)
  }

  override def dispose(): Unit = {
    patch.dispose()
  }

}
