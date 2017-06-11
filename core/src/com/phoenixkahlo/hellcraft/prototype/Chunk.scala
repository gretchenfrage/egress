package com.phoenixkahlo.hellcraft.prototype

import com.phoenixkahlo.hellcraft.util._

import scala.collection.mutable

class Chunk(
             val chunkCoords: V3I,
             val world: World
           ) {

  var versionID: Long = 0 // every time this is mutated, the versionID is incremented
  val blocks = new Array[Byte](world.chunkSize * world.chunkSize * world.chunkSize)
  val history: mutable.SortedMap[Long, V3I] = new mutable.TreeMap()

  private def compress(v: V3I): Int =
    v.xi + v.zi * world.chunkSize + v.yi * world.chunkSize * world.chunkSize

  /**
    * Gets the block at the given local coordinates. This will work even if the block is outside of this chunk.
    */
  def apply(v: V3I): Option[Block] =
    if (v >= Origin && v < world.chunkSizeVec)
      Some(BlockDirectory.lookup(blocks(compress(v))))
    else
      world.block(chunkCoords * world.chunkSize + v)

  def set(v: V3I, block: Block): Unit = {
    blocks.update(compress(v), block.id)
    versionID += 1
    history.put(versionID, v)
  }


}
