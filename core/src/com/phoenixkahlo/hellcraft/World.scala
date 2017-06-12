package com.phoenixkahlo.hellcraft

import com.phoenixkahlo.hellcraft.util.V3I

/**
  * Minimal interface to world state
  */
trait World {

  def blockAt(v: V3I): Option[Block]

}
