package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.esotericsoftware.kryo.DefaultSerializer
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.math.{Origin, Repeated, V3I}
import com.phoenixkahlo.hellcraft.save.ChunkSerializer
import com.phoenixkahlo.hellcraft.util.{ParamCache, RNG}

import scala.collection.immutable.HashMap

/**
  * A unit of world.
  */
@DefaultSerializer(classOf[ChunkSerializer])
class Chunk (
                     val pos: V3I, // coordinates in chunks, not blocks
                     val size: Int,
                     val blocks: Vector[Byte],
                     val entities: Map[UUID,Entity],
                     @transient val lastRenderer: ParamCache[(TexturePack, World), ChunkRenderer],
                     val lastRendererDirty: Boolean
                   ) {


  def this(pos: V3I, size: Int = 16) = this(
    pos, size,
    (1 to size * size * size).foldLeft(Vector[Byte]())((v, _) => v :+ 0.toByte),
    new HashMap,
    null,
    true
  )

  def copy(
            pos: V3I = this.pos,
            size: Int = this.size,
            blocks: Vector[Byte] = this.blocks,
            entities: Map[UUID,Entity] = this.entities,
            lastRenderer: ParamCache[(TexturePack, World), ChunkRenderer] = this.renderer,
            lastRendererDirty: Boolean = false
          ): Chunk = new Chunk(
    pos,
    size,
    blocks,
    entities,
    lastRenderer,
    lastRendererDirty
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

  /**
    * warning: this will not cause adjacent chunks to recompile their meshes
    * also, remember that this uses chunk coordinates, not world coordinates
    * you should probably use World.putBlock instead
    */
  def putBlock(v: V3I, b: Block): Chunk = copy(
    blocks = blocks.updated(compress(v), b.id),
    lastRendererDirty = true
  )

  def renderUncached: Chunk = copy(lastRendererDirty = true)

  def putEntity(entity: Entity): Chunk = copy(
    entities = entities.updated(entity.id, entity)
  )

  def removeEntity(id: UUID): Chunk = copy(
    entities = entities - id
  )

  def removeEntity(entity: Entity): Chunk = removeEntity(entity.id)

  def update(world: World): Seq[ChunkEvent] = {
    val idStreams: Stream[Stream[UUID]] = Stream.iterate(Stream.iterate(UUID.randomUUID())(_ => UUID.randomUUID()))(_ => Stream.iterate(UUID.randomUUID())(_ => UUID.randomUUID()))
    entities.values.zip(idStreams).flatMap({ case (entity, ids) => entity.update(world, ids) }).toSeq
  }

  @transient private lazy val renderer: ParamCache[(TexturePack, World), ChunkRenderer] =
    if (lastRenderer == null)
      new ParamCache({ case (textures, world) => new ChunkRenderer(this, textures, world, None) })
    else {
      if (lastRendererDirty)
        new ParamCache({ case params@(textures, world) => new ChunkRenderer(this, textures, world, Some(lastRenderer(params)))})
      else
        lastRenderer
    }

  def renderables(texturePack: TexturePack, world: World): Seq[RenderableFactory] =
    entities.values.flatMap(_.renderables(texturePack)).toSeq :+ renderer((texturePack, world))

  def mapBlocks(f: V3I => Block): Chunk =
    copy(
      blocks = blocks.indices.par.map(decompress).map(f).map(_.id).seq.to[Vector],
      lastRenderer = null
    )

}