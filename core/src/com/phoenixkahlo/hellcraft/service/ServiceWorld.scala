package com.phoenixkahlo.hellcraft.service

import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.math.V3I

trait ServiceWorld {
  def chunk(p: V3I): Option[Chunk]
}
