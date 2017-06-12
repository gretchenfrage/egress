package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.graphics.g3d.{Renderable, RenderableProvider}
import com.badlogic.gdx.utils.Pool
import com.badlogic.gdx.utils
import com.phoenixkahlo.hellcraft.util.{Origin, Repeated, V3I}

/**
  * A unit of world.
  */
class Chunk private (
             val pos: V3I, // coordinates in chunks, not blocks
             val size: Int,
             val blocks: Vector[Byte],
             val entities: Vector[Entity],
             var blocksRenderableCache: Option[RenderableFactory]
           ) {

  def this(pos: V3I, size: Int) = this(
    pos, size,
    (1 to size * size * size).foldLeft(Vector[Byte]())((v, _) => v :+ 0.toByte),
    Vector(),
    None
  )

  def copy(
                    pos: V3I = this.pos,
                    size: Int = this.size,
                    blocks: Vector[Byte] = this.blocks,
                    entities: Vector[Entity] = this.entities,
                    blocksRenderableCache: Option[RenderableFactory] = this.blocksRenderableCache
                  ): Chunk = new Chunk(
    pos,
    size,
    blocks,
    entities,
    blocksRenderableCache
  )

  private def compress(v: V3I): Int = v.xi + v.zi * size + v.yi * size * size

  def apply(v: V3I): Option[Block] =
    if (v >= Origin && v < Repeated(size)) Some(BlockDirectory.lookup(blocks(compress(v))))
    else None

  def put(v: V3I, b: Block): Chunk = copy(
    blocks = blocks.updated(compress(v), b.id),
    blocksRenderableCache = None
  )

  def renderables(texturePack: TexturePack): Seq[RenderableFactory] = {
    blocksRenderableCache match {
      case Some(_) =>
      case None => blocksRenderableCache = Some(ChunkRenderer(this, texturePack))
    }
    entities.flatMap(_.renderables(texturePack)) :+ blocksRenderableCache.get
  }

  def mapBlocks(f: V3I => Block): Chunk =
    (Origin until V3I(size, size, size)).foldLeft(this)({ case (c, v) => c.put(v, f(v)) })

}