package com.phoenixkahlo.hellcraft.singleplayer

import com.phoenixkahlo.hellcraft.core.{Chunk, Densities, World}
import com.phoenixkahlo.hellcraft.math.{Simplex, V2F, V3I}
import com.phoenixkahlo.hellcraft.util.fields.FractionField
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

class Generator(res: Int) {

  val noise = Simplex(10, 1)

  def genChunk(p: V3I): Fut[Chunk] = {
    Fut(new Chunk(p, Densities(p, FractionField(V3I(res, res, res), i => {
      val v = p * res + i
      if (-v.yi > v.flatten.dist(V2F(0, 0))) 1
      else 0
    }))), UniExecutor.exec(p * 16 + V3I(8, 8, 8)))
    /*
    Fut({
      new Chunk(p, Densities(p, FractionField(V3I(res, res, res), v => noise(p * 16 + (v / res * 16)))))
    }, UniExecutor.exec(p * 16 + V3I(8, 8, 8)))
    */
  }

}
