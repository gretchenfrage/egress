package com.phoenixkahlo.hellcraft.menu.util

import com.badlogic.gdx.graphics.{Pixmap, Texture}
import com.badlogic.gdx.graphics.g2d.{Batch, NinePatch}
import com.badlogic.gdx.scenes.scene2d.Actor
import com.badlogic.gdx.scenes.scene2d.ui.ScrollPane
import com.badlogic.gdx.utils.Disposable

case class EScrollerStyle(
                           patch: Pixmap,
                           depth: Int,
                           width: Int,
                           height: Int
                         )

class EScroller(widget: Actor, style: EScrollerStyle) extends ScrollPane(widget) with Disposable {

  val patch = new FrameFactory(style.patch, style.depth)(style.width, style.height)
  val border = new FrameFactory(style.patch, style.depth)(style.width, style.height, true)
  setSize(style.width, style.height)
  setFadeScrollBars(false)

  override def draw(batch: Batch, parentAlpha: Float): Unit = {
    batch.draw(patch, getX, getY, getWidth, getHeight)
    super.draw(batch, parentAlpha)
    batch.draw(border, getX, getY, getWidth, getHeight)
  }

  override def dispose(): Unit = {
    patch.dispose()
  }

}
