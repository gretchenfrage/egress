package com.phoenixkahlo.hellcraft.core

import java.util.{Objects, UUID}

import com.esotericsoftware.kryo.DefaultSerializer
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.graphics.{ChunkRenderer, RenderableFactory, ResourcePack}
import com.phoenixkahlo.hellcraft.math.{Origin, Repeated, V3I}
import com.phoenixkahlo.hellcraft.util.{ParamCache, RNG}

import scala.collection.immutable.HashMap

/**
  * A unit of world.
  */
class Chunk (
              val pos: V3I, // coordinates in chunks, not blocks
              val size: Int,
              val blocks: BlockGrid,
              //val blocks: Vector[Byte],
              val entities: Map[UUID,Entity],
              @transient val lastRenderer: ParamCache[(ResourcePack, World), ChunkRenderer],
              val lastRendererDirty: Boolean
                   ) {

  def this(pos: V3I, size: Int = 16) = this(
    pos, size,
    BlockGrid(Air),
    //(1 to size * size * size).foldLeft(Vector[Byte]())((v, _) => v :+ 0.toByte),
    new HashMap,
    null,
    true
  )

  def copy(
            pos: V3I = this.pos,
            size: Int = this.size,
            blocks: BlockGrid = this.blocks,
            entities: Map[UUID,Entity] = this.entities,
            lastRenderer: ParamCache[(ResourcePack, World), ChunkRenderer] = this.renderer,
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
    blocks(v)

  /**
    * warning: this will not cause adjacent chunks to recompile their meshes
    * also, remember that this uses chunk coordinates, not world coordinates
    * you should probably use World.putBlock instead
    */
  def putBlock(v: V3I, b: Block): Chunk = copy(
    blocks = blocks.updated(v, b),
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
    val seed: Long = (world.time.hashCode().toLong << 32) | pos.hashCode().toLong
    val idStreams: Stream[Stream[UUID]] = RNG.meta(RNG(seed), RNG.uuids)
    entities.values.zip(idStreams).flatMap({ case (entity, ids) => entity.update(world, ids) }).toSeq
  }

  @transient private lazy val renderer: ParamCache[(ResourcePack, World), ChunkRenderer] =
    if (lastRenderer == null)
      new ParamCache({ case (textures, world) => new ChunkRenderer(this, textures, world, None) })
    else {
      if (lastRendererDirty)
        new ParamCache({ case params@(textures, world) => new ChunkRenderer(this, textures, world, Some(lastRenderer(params)))})
      else
        lastRenderer
    }

  def renderables(texturePack: ResourcePack, world: World): Seq[RenderableFactory] =
    entities.values.flatMap(_.renderables(texturePack)).toSeq :+ renderer((texturePack, world))

  def mapBlocks(f: V3I => Block): Chunk =
    copy(
      blocks = BlockGrid(f),
      lastRenderer = null
    )

  override def hashCode(): Int =
    Objects.hash(pos, new Integer(size), blocks, entities)

  override def equals(o: Any): Boolean = o match {
    case chunk: Chunk => pos == chunk.pos && size == chunk.size && blocks == chunk.blocks && entities == chunk.entities
    case _ => false
  }

}