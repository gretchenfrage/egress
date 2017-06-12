package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.graphics.g3d.{Renderable, RenderableProvider}
import com.badlogic.gdx.utils.{Array, Pool}
import com.phoenixkahlo.hellcraft.util.{Origin, V3I}

class FiniteWorld(
                   val size: V3I,
                   val chunkSize: Int,
                   val chunks: Vector[Chunk]
                 ) extends World {

  def this(size: V3I, chunkSize: Int) =
    this(
      size, chunkSize,
      (Origin until size map (new Chunk(_, chunkSize))).to[Vector]
    )

  def compress(v: V3I): Int =
    v.xi + v.zi * size.xi + v.yi * size.xi * size.zi

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
    (Origin until size).foldLeft(this)({ case (w, v) => w.updateChunk(v, f)})

  def mapBlocks(f: V3I => Block): FiniteWorld =
    (Origin until size * chunkSize).foldLeft(this)({ case (w, v) => w.updateBlock(v, f(v)) })

  def renderables(texturePack: TexturePack): Seq[RenderableFactory] =
    chunks.flatMap(_.renderables(texturePack, this))


}
