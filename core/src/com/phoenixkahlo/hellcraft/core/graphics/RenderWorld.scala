package com.phoenixkahlo.hellcraft.core.graphics

import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.core.entity.AnyEnt

trait RenderWorld {
  def renderableChunks: Seq[Chunk]

  def renderableEnts: Seq[AnyEnt]

  def ftime: Float

  def interp: Float
}