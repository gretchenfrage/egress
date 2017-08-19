package com.phoenixkahlo.hellcraft.graphics.`new`

import com.phoenixkahlo.hellcraft.core.{Chunk, World}
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.util.ParamCache

class ChunkRenderer(chunk: Chunk) {

  val mesh = new ParamCache[(World, ResourcePack), RenderUnit]({ case (world, pack) => {
    if (!chunk.pos.neighbors.forall(world.chunkAt(_).isDefined))
      throw new IllegalArgumentException("chunk cannot render with undefined neighbors")

    ???
  }})

  def apply(world: World, pack: ResourcePack): Seq[RenderUnit] = {
    if (chunk.pos.neighbors.forall(world.chunkAt(_).isDefined)) Seq(mesh((world, pack)))
    else Seq.empty
  }

}
