package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.graphics.g3d.{Renderable, RenderableProvider}
import com.badlogic.gdx.utils.Pool
import com.badlogic.gdx.utils
import com.phoenixkahlo.hellcraft.util.{Origin, ParamCache, Repeated, V3I}

/**
  * A unit of world.
  */
class Chunk private(
                     val pos: V3I, // coordinates in chunks, not blocks
                     val size: Int,
                     val blocks: Vector[Byte],
                     val entities: Vector[Entity],
                     @transient val lastRenderer: ParamCache[(TexturePack, World), ChunkRenderer]
                   ) {


  def this(pos: V3I, size: Int) = this(
    pos, size,
    (1 to size * size * size).foldLeft(Vector[Byte]())((v, _) => v :+ 0.toByte),
    Vector(),
    null
  )

  def copy(
            pos: V3I = this.pos,
            size: Int = this.size,
            blocks: Vector[Byte] = this.blocks,
            entities: Vector[Entity] = this.entities,
            lastRenderer: ParamCache[(TexturePack, World), ChunkRenderer] = this.lastRenderer
          ): Chunk = new Chunk(
    pos,
    size,
    blocks,
    entities,
    lastRenderer
  )

  def compress(v: V3I): Int = v.xi + v.zi * size + v.yi * size * size

  def decompress(i: Int): V3I = {
    val y = i / (size * size)
    val z = (i % (size * size)) / size
    val x = i % size
    V3I(x, y, z)
  }

  def apply(v: V3I): Option[Block] =
    if (v >= Origin && v < Repeated(size)) Some(BlockDirectory.lookup(blocks(compress(v))))
    else None

  def updateBlock(v: V3I, b: Block): Chunk = copy(
    blocks = blocks.updated(compress(v), b.id),
    lastRenderer = null
  )

  @transient private lazy val renderer: ParamCache[(TexturePack, World), ChunkRenderer] = lastRenderer match {
    case null => new ParamCache({ case (t, w) => new ChunkRenderer(this, t, w) })
    case r => r
  }

  def renderables(texturePack: TexturePack, world: World): Seq[RenderableFactory] = {
    entities.flatMap(_.renderables(texturePack)) :+ renderer((texturePack, world))
  }

  def mapBlocks(f: V3I => Block): Chunk =
    copy(
    blocks = blocks.indices.par.map(decompress).map(f).map(_.id).seq.to[Vector],
    lastRenderer = null
  )
    //(Origin until V3I(size, size, size)).foldLeft(this)({ case (c, v) => c.updateBlock(v, f(v)) })

}