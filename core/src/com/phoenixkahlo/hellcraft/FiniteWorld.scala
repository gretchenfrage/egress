package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.graphics.g3d.{Renderable, RenderableProvider}
import com.badlogic.gdx.utils.{Array, Pool}
import com.phoenixkahlo.hellcraft.util.{Origin, V3I}
/*
private object Decompress {

  def apply(i: Int, size: V3I): V3I = {
    val y = i / (size.xi * size.zi)
    val z = (i % (size.xi * size.zi)) / size.xi
    val x = i % size.xi
    V3I(x, y, z)
  }

}
*/
class FiniteWorld(
                   val size: V3I,
                   val chunkSize: Int,
                   val chunks: Vector[Chunk]
                 ) extends World {

  def this(size: V3I, chunkSize: Int) =
    this(
      size, chunkSize,
      (0 until size.fold(_ * _)).map(n => new Chunk(size.decompress(n), chunkSize)).to[Vector]
    )

  def compress(v: V3I): Int =
    v.xi + v.zi * size.xi + v.yi * size.xi * size.zi

  def decompress(i: Int): V3I = size.decompress(i)

  def chunkAt(v: V3I): Option[Chunk] =
    if (v >= Origin && v < size) Some(chunks(compress(v)))
    else None

  override def blockAt(v: V3I): Option[Block] = {
    chunkAt(v / chunkSize floor).flatMap(_ (v % chunkSize))
  }

  def updateChunk(v: V3I, f: Chunk => Chunk): FiniteWorld =
    new FiniteWorld(
      size, chunkSize,
      chunks.updated(
        compress(v),
        f(chunkAt(v).get)
      )
    )

  def updateBlock(v: V3I, b: Block): FiniteWorld =
    updateChunk(v / chunkSize floor, _.updateBlock(v % chunkSize, b))

  def mapChunks(f: Chunk => Chunk): FiniteWorld =
    new FiniteWorld(
      size, chunkSize,
      chunks.par.map(f).seq
    )

  def mapBlocks(f: V3I => Block): FiniteWorld =
    mapChunks(chunk => chunk.mapBlocks(v => f(v + (chunk.pos * chunkSize))))

  def renderables(texturePack: TexturePack): Seq[RenderableFactory] =
    chunks.flatMap(_.renderables(texturePack, this))


}