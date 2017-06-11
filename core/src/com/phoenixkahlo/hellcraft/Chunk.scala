package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.graphics.g3d.{Renderable, RenderableProvider}
import com.badlogic.gdx.utils.Pool
import com.badlogic.gdx.utils
import com.phoenixkahlo.hellcraft.util.{Origin, Repeated, V3I}

/**
  * A unit of world.
  */
case class Chunk(
                  pos: V3I, // coordinates in chunks, not blocks
                  size: Int,
                  blocks: Vector[Byte],
                  entities: Vector[Entity],
                  blocksRenderableCache: Option[RenderableFactory]
                ) {

  def compress(v: V3I): Int = v.xi + v.zi * size + v.yi * size * size

  def apply(v: V3I): Option[Block] =
    if (v >= Origin && v < Repeated(size)) Some(BlockDirectory.lookup(blocks(compress(v))))
    else None

  def put(v: V3I, b: Block): Chunk = copy(
    blocks = blocks.updated(compress(v), b.id),
    blocksRenderableCache = None
  )

  def renderables: Seq[() => Renderable] = {
    val blocksRenderable: RenderableFactory = blocksRenderableCache match {
      case Some(r) => r
      case None => ChunkRenderer(this)
    }
    entities.flatMap(_.renderables) :+ blocksRenderable
  }

}

object Chunk {

  def apply(pos: V3I, size: Int = 16) =
    Chunk(
      pos,
      size,
      (1 to size * size * size).foldLeft(Vector[Byte]())((v, _) => v :+ 0),
      Vector(),
      None
    )

}