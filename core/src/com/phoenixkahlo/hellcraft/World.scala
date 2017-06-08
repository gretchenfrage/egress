package com.phoenixkahlo.hellcraft

import com.phoenixkahlo.hellcraft.util.{Origin, V3I}

import scala.collection.mutable

class World(
           val size: V3I, // size in chunks, not in blocks
           val chunkSize: Int = 16
           ) {

  def this(x: Int, y: Int, z: Int) =
    this(V3I(x, y, z))

  val chunks = new Array[Chunk](size.monoidFold(_ * _))
  for (v <- Origin until size)
    chunks.update(compress(v), new Chunk(v, this))
  val balls = new mutable.HashSet[Ball]()

  private def compress(v: V3I): Int =
    v.xi + v.zi * size.xi + v.yi * size.xi * size.zi

  /**
    * Get the chunk at the given chunk coordinates
    */
  def chunk(v: V3I): Option[Chunk] =
    if (v >= Origin && v < size)
      Some(chunks(compress(v)))
    else
      None

  /**
    * Get the block at the given global coordinates
    */
  def block(v: V3I): Option[Block] =
    if (v.x < 0 || v.y < 0 || v.z < 0)
      None
    else
      chunk((v / chunkSize).toInts).flatMap(_(v % chunkSize))

  def set(v: V3I, block: Block): Unit =
    if (v >= Origin)
      chunk((v / chunkSize).toInts).get.set(v % chunkSize, block)

  def blockSize = size * chunkSize

  val chunkSizeVec = V3I(chunkSize, chunkSize, chunkSize)

}
