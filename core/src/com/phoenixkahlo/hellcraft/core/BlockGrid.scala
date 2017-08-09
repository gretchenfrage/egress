package com.phoenixkahlo.hellcraft.core

import com.phoenixkahlo.hellcraft.math.{Origin, V3I}

case class BlockGrid(blocks: Vector[Byte]) {

  def updated(v: V3I, b: Block): BlockGrid = {
    new BlockGrid(blocks.updated(BlockGrid.compress(v), b.id))
  }

  def apply(v: V3I): Option[Block] =
    if (v >= Origin && v < V3I(16, 16, 16)) Some(BlockDirectory(blocks(BlockGrid.compress(v))))
    else None

}

object BlockGrid {

  def apply(block: Block): BlockGrid = {
    this((1 to 4096).foldLeft(Vector[Byte]())((v, _) => v :+ block.id))
  }

  def apply(generator: V3I => Block): BlockGrid = {
    this((0 until 4096).map(i => generator(decompress(i)).id).to[Vector])
  }

  def compress(v: V3I): Int =
    v.xi + v.zi * 16 + v.yi * 256

  def decompress(i: Int): V3I =
    V3I(
      x = i % 16,
      y = i / 256,
      (i % 256) / 16
    )

}