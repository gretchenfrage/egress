package com.phoenixkahlo.hellcraft

import com.phoenixkahlo.hellcraft.util._

import scala.collection.mutable

class Chunk(val size: V3I) {

  def this(x: Int, y: Int, z: Int) =
    this(V3I(x, y, z))

  var versionID: Long = 0 // every time this is mutated, the versionID is incremented
  val blocks = new Array[Byte](size.monoidFold(_ * _))
  val history: mutable.SortedMap[Long, V3I] = new mutable.TreeMap()

  private def compress(v: V3I): Int =
    v.xi + v.zi * size.xi + v.yi * size.xi * size.zi

  def apply(v: V3I): Option[Block] =
    if (v >= Origin && v < size)
      Some(BlockDirectory.lookup(blocks(compress(v))))
    else
      None

  def set(v: V3I, block: Block): Unit = {
    blocks.update(compress(v), block.id)
    versionID += 1
    history.put(versionID, v)
  }


}
