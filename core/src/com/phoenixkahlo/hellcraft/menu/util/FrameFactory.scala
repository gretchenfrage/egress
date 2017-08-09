package com.phoenixkahlo.hellcraft.menu.util

import com.badlogic.gdx.graphics.Pixmap.Blending
import com.badlogic.gdx.graphics.{Pixmap, Texture}
import com.badlogic.gdx.graphics.g2d.TextureRegion

class FrameFactory(patch: Pixmap, depth: Int) {

  def apply(width: Int, height: Int, hollow: Boolean = false): Texture = {
    val canvas = new Pixmap(width, height, Pixmap.Format.RGBA8888)
    canvas.setBlending(Blending.None)

    // draw the center
    val centerWidth = patch.getWidth - depth * 2
    val centerHeight = patch.getHeight - depth * 2
    if (!hollow) {
      for {
        x <- Range(depth, width - depth, centerWidth)
        y <- Range(depth, height - depth, centerHeight)
      } yield {
        canvas.drawPixmap(patch, depth, depth, centerWidth, centerHeight, x, y, centerWidth, centerHeight)
      }
    }
    // draw the sides
    for (x <- depth until width - depth) {
      // bottom side
      canvas.drawPixmap(patch, x % centerWidth + depth, 0, 1, depth, x, 0, 1, depth)
      // top side
      canvas.drawPixmap(patch, x % centerWidth + depth, patch.getHeight - depth, 1, depth, x, height - depth, 1, depth)
    }
    for (y <- depth until height - depth) {
      // left side
      canvas.drawPixmap(patch, 0, y % centerHeight + depth, depth, 1, 0, y, depth, 1)
      // right side
      canvas.drawPixmap(patch, patch.getWidth - depth, y % centerHeight + depth, depth, 1, width - depth, y, depth, 1)
    }
    // draw the corners
    // bottom-left corner
    canvas.drawPixmap(patch, 0, 0, depth, depth, 0, 0, depth, depth)
    // bottom-right corner
    canvas.drawPixmap(patch, patch.getWidth - depth, 0, depth, depth, width - depth, 0, depth, depth)
    // upper-left corner
    canvas.drawPixmap(patch, 0, patch.getHeight - depth, depth, depth, 0, height - depth, depth, depth)
    // upper-right corner
    canvas.drawPixmap(patch, patch.getWidth - depth, patch.getHeight - depth, depth, depth, width - depth,
      height - depth, depth, depth)

    // upload to VRAM and return
    new Texture(canvas)
  }

}
