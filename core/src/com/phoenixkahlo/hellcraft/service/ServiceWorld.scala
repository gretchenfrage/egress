package com.phoenixkahlo.hellcraft.service

import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.threading.Fut

trait ServiceWorld {
  def chunkFut(p: V3I): Fut[Option[Chunk]]
}
