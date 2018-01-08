package com.phoenixkahlo.hellcraft.core.graphics

import com.phoenixkahlo.hellcraft.core.Chunk

trait RenderWorld {
  def renderableChunks: Seq[Chunk]

  def ftime: Float

  def interp: Float
}