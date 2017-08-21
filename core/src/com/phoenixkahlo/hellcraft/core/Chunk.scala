package com.phoenixkahlo.hellcraft.core

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.graphics.`new`.ChunkRenderer
import com.phoenixkahlo.hellcraft.math.V3I

@CarboniteWith(classOf[FieldNode])
class Chunk(
           val pos: V3I,
           val densities: FractionField
           ) {

  val terrain: Terrain = new Terrain(this)
  val renderer: ChunkRenderer = new ChunkRenderer(this)

}
