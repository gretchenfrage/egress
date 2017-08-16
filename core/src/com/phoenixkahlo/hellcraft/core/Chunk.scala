package com.phoenixkahlo.hellcraft.core

import java.util.{Objects, UUID}

import com.esotericsoftware.kryo.DefaultSerializer
import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.graphics.{ChunkRenderer, RenderableFactory, ResourcePack}
import com.phoenixkahlo.hellcraft.math.{Origin, Repeated, V3I}
import com.phoenixkahlo.hellcraft.util.{ParamCache, Profiler, RNG}

import scala.collection.immutable.HashMap

/**
  * A unit of world.
  */
@CarboniteWith(classOf[FieldNode])
class Chunk (
              val pos: V3I,
              val blocks: BlockGrid,
              val entities: Map[UUID,Entity],
              @transient val lastRenderer: ParamCache[(ResourcePack, World), ChunkRenderer],
              @transient val lastMeshableFlag: ParamCache[V3I => Option[Chunk], Boolean],
              val lastGraphicsDirty: Boolean,
              val freshlyLoaded: Boolean = true
            ) {


  def this(pos: V3I, blocks: BlockGrid) =
    this(pos, blocks, Map.empty, null, null, true)

  def this(pos: V3I, generator: V3I => Block) =
    this(pos, BlockGrid(v => generator(v + (pos * 16))), Map.empty, null, null, true)

  def copy(
            pos: V3I = this.pos,
            blocks: BlockGrid = this.blocks,
            entities: Map[UUID,Entity] = this.entities,
            lastRenderer: ParamCache[(ResourcePack, World), ChunkRenderer] = this.renderer,
            lastMeshableFlag: ParamCache[V3I => Option[Chunk], Boolean] = this.lastMeshableFlag,
            lastGraphicsDirty: Boolean = false,
            freshlyLoaded: Boolean = false
          ): Chunk = new Chunk(
    pos,
    blocks,
    entities,
    lastRenderer,
    lastMeshableFlag,
    lastGraphicsDirty,
    freshlyLoaded
  )

  def compress(v: V3I): Int = v.xi + v.zi * 16 + v.yi * 256

  def decompress(i: Int): V3I = {
    val y = i / (256)
    val z = (i % (256)) / 16
    val x = i % 16
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
    lastGraphicsDirty = true
  )

  def invalidateGraphics: Chunk = copy(lastGraphicsDirty = true)

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

  @transient private lazy val renderer: ParamCache[(ResourcePack, World), ChunkRenderer] = {
    val p = Profiler("chunk renderer lazy eval")
    try {
      if (lastRenderer == null)
        new ParamCache({ case (textures, world) => {
          val p = Profiler("chunk renderer construct, without prior")
          try new ChunkRenderer(this, textures, world, None)
          finally {
            p.log()
            p.printDisc(1)
          }
        } })
      else {
        if (lastGraphicsDirty)
          new ParamCache({ case params@(textures, world) => {
            val p = Profiler("chunk renderer construct, with prior")
            try new ChunkRenderer(this, textures, world, Some(lastRenderer(params)))
            finally {
              p.log()
              p.printDisc(1)
            }
          } })
        else
          lastRenderer
      }
    } finally p.printDisc(1)
  }

  @transient private lazy val meshableFlag: ParamCache[V3I => Option[Chunk], Boolean] =
    if (lastMeshableFlag != null && !lastGraphicsDirty) lastMeshableFlag
    else new ParamCache((getChunk: V3I => Option[Chunk]) => {
      if (blocks isAllTranslucent) false
      else if (blocks.isAllOpaque && {
        val adj = pos.touching.flatMap(getChunk(_))
        if (adj.size != 6) println("warn: chunk meshable flag computed without all neighbors loaded")
        (adj.size == 6) && adj.forall(_.blocks.isAllOpaque)
      }) false
      else true
    })

  def isMeshable(getChunk: V3I => Option[Chunk]): Boolean =
    meshableFlag(getChunk)

  def renderables(texturePack: ResourcePack, world: World): Seq[RenderableFactory] = {
    val p = Profiler("chunk " + pos)
    try {
      //entities.values.flatMap(_.renderables(texturePack)).toSeq :+ renderer((texturePack, world))
      val entRen = entities.values.flatMap(_.renderables(texturePack)).toSeq
      p.log()
      val chuRen = renderer((texturePack, world))
      p.log()
      entRen :+ chuRen
    } finally p.printDisc(2)
  }

  override def hashCode(): Int =
    Objects.hash(pos, blocks, entities)

  override def equals(o: Any): Boolean = o match {
    case chunk: Chunk => pos == chunk.pos && blocks == chunk.blocks && entities == chunk.entities
    case _ => false
  }

  override def toString: String =
    "(pos=" + pos + ", entities=" + entities + ", blocks=" + blocks + ")"

}