package com.phoenixkahlo.hellcraft.singleplayer

import com.phoenixkahlo.hellcraft.core.{Chunk, World}
import com.phoenixkahlo.hellcraft.math.{Simplex, V3I}
import com.phoenixkahlo.hellcraft.util.fields.FractionField
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

class Generator(res: Int) {

  val noise = Simplex(10, 1)

  def genChunk(p: V3I): Fut[Chunk] = {
    Fut({
      new Chunk(p, FractionField(V3I(res, res, res), v => noise(p * res + v)))
    }, UniExecutor.exec(p * 16 + V3I(8, 8, 8)))
  }

}
