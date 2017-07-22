package com.phoenixkahlo.hellcraft.core.entity

import com.phoenixkahlo.hellcraft.math.{V3F, V3I}

trait PositionHaver extends Entity{

  def pos: V3F

  def chunkPos: V3I = pos / 16 floor

}
