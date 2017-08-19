package com.phoenixkahlo.hellcraft.core

import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.oldcore.Chunk

trait World {

  def chunkAt(p: V3I): Option[Chunk]

  def time: Long

  def res: V3I

}
