package com.phoenixkahlo.hellcraft.isotest

import com.phoenixkahlo.hellcraft.core.{Chunk, World}
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.graphics.`new`.{ChunkMesher, RenderUnit}
import com.phoenixkahlo.hellcraft.math.{Ones, Origin, V3I}
import com.phoenixkahlo.hellcraft.util.fields.FractionField

class ArrayWorld(val gen: V3I => Float, size: V3I = V3I(6, 6, 6), override val res: Int = 32) extends World {

  val chunks = new Array[Chunk](size.fold(_ * _))
  for (p <- Origin until size) {
    chunks(size.compress(p)) = new Chunk(p, FractionField(resVec, v => gen(p * resVec.dim.get + v)))
  }

  override def chunkAt(p: V3I): Option[Chunk] = {
    val i = size.compress(p)
    if (chunks.indices contains i) Some(chunks(i))
    else None
  }

  override val time: Long = 0

  def renderables(pack: ResourcePack): Seq[RenderUnit] = {
    (V3I(2, 2, 2) until (size - V3I(2, 2, 2) )).map(chunkAt(_).get.mesher).flatMap(_(this, pack))
  }


}
