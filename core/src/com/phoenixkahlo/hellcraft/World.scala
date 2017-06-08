package com.phoenixkahlo.hellcraft

import com.phoenixkahlo.hellcraft.util.{Origin, V3I}

class World(
           val size: V3I, // size in chunks, not in blocks
           val chunkSize: Int = 16
           ) {

  def this(x: Int, y: Int, z: Int) =
    this(V3I(x, y, z))

  val chunks = new Array[Chunk](size.monoidFold(_ * _))
  for (i <- 0 until chunks.length)
    chunks.update(i, new Chunk(V3I(chunkSize, chunkSize, chunkSize)))

  private def compress(v: V3I): Int =
    v.xi + v.zi * size.xi + v.yi * size.xi * size.zi

  def apply(v: V3I): Option[Chunk] =
    if (v >= Origin && v < size)
      Some(chunks(compress(v)))
    else
      None

}
