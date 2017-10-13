package com.phoenixkahlo.hellcraft.core

import java.util.{Objects, UUID}

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.math.{Origin, RNG, V3F, V3I}
import com.phoenixkahlo.hellcraft.util.fields.{ByteFractionField, ByteFractionFieldBuffer, OptionField}

@CarboniteWith(classOf[FieldNode])
class Chunk(
             val pos: V3I,
             val terrain: Terrain,
             val entities: Map[UUID, Entity] = Map.empty,
             val terrainSoup: Option[TerrainSoup] = None,
             val blockSoup: Option[BlockSoup] = None,
             @transient lastTerrainMesher: TerrainMesher = null,
             @transient lastBlockMesher: BlockMesher = null
           ) {


  @transient lazy val terrainMesher: Option[TerrainMesher] = Option(lastTerrainMesher) match {
    case last if last isDefined => last
    case None => terrainSoup.map(new TerrainMesher(this, _))
  }

  @transient lazy val blockMesher: Option[BlockMesher] = Option(lastBlockMesher) match {
    case last if last isDefined => last
    case None => blockSoup.map(new BlockMesher(this, _))
  }

  def putEntity(entity: Entity): Chunk =
    new Chunk(pos, terrain, entities + (entity.id -> entity), terrainSoup, blockSoup, terrainMesher.orNull, blockMesher.orNull)

  def removeEntity(entity: UUID): Chunk =
    new Chunk(pos, terrain, entities - entity, terrainSoup, blockSoup, terrainMesher.orNull, blockMesher.orNull)

  def setTerrain(neu: Terrain): Chunk =
    new Chunk(pos, neu, entities, None, None, null, null)

  def setTerrainSoup(ts: TerrainSoup): Chunk =
    new Chunk(pos, terrain, entities, Some(ts), blockSoup, null, blockMesher.orNull)

  def setBlockSoup(bs: BlockSoup): Chunk =
    new Chunk(pos, terrain, entities, terrainSoup, Some(bs), terrainMesher.orNull, null)

  def invalidate: Chunk =
    new Chunk(pos, terrain, entities, None, None, null, null)

  def isComplete: Boolean =
    terrainSoup.isDefined && blockSoup.isDefined

  def makeComplete(world: World): Chunk = {
    val ts = TerrainSoup(terrain, world).get
    val bs = BlockSoup(terrain, world).get
    new Chunk(pos, terrain, entities, Some(ts), Some(bs), null, null)
  }

  def update(world: World): Seq[UpdateEffect] = {
    val seed: Long = (world.time.hashCode().toLong << 32) | pos.hashCode().toLong
    val idss: Stream[Stream[UUID]] = RNG.meta(RNG(seed), RNG.uuids)
    entities.values.zip(idss).flatMap({ case (entity, ids) => entity.update(world, ids) }).toSeq
  }

  override def hashCode(): Int =
    Objects.hash(pos, terrain, entities)

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case chunk: Chunk => pos == chunk.pos && terrain == chunk.terrain && entities == chunk.entities
      case _ => false
    }

  def renderables(pack: ResourcePack, world: World): Seq[RenderUnit] = {
    entities.values.flatMap(_.renderables(pack)).toSeq ++
      terrainMesher.map(_(world, pack)).getOrElse(Seq.empty) ++
      blockMesher.map(_(world, pack)).getOrElse(Seq.empty)
  }

}