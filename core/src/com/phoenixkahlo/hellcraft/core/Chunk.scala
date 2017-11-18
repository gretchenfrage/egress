package com.phoenixkahlo.hellcraft.core

import java.util.{Objects, UUID}

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.math.physics._
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc
import com.phoenixkahlo.hellcraft.util.fields.{ByteFractionField, ByteFractionFieldBuffer, OptionField}

@CarboniteWith(classOf[FieldNode])
class Chunk(
             val pos: V3I,
             val terrain: Terrain,
             val entities: Map[UUID, Entity] = Map.empty,
             val terrainSoup: Option[TerrainSoup] = None,
             val blockSoup: Option[BlockSoup] = None,
             @transient lastBroadphase: Broadphase = null,
             @transient lastTerrainMesher: TerrainMesher = null,
             @transient lastBlockMesher: BlockMesher = null,
             @transient lastTerrainValid: Boolean = true,
             @transient lastBlockValid: Boolean = true
           ) extends Serializable {


  // TODO: there's no point in declaring the graphics values lazy if they get evaluated whenever the chunk is transformed
  @transient lazy val terrainMesher: Option[TerrainMesher] = Option(lastTerrainMesher) match {
    case last if last.isDefined && lastTerrainValid => last
    case last => terrainSoup.map(new TerrainMesher(this, _)) match {
      case neu if neu isDefined => neu
      case _ => last
    }
  }

  @transient lazy val blockMesher: Option[BlockMesher] = Option(lastBlockMesher) match {
    case last if last.isDefined && lastBlockValid => last
    case last => blockSoup.map(new BlockMesher(this, _)) match {
      case neu if neu isDefined => neu
      case _ => last
    }
  }

  @transient lazy val broadphase: Broadphase = Option(lastBroadphase).getOrElse({
    (terrainSoup, blockSoup) match {
      case (Some(ts), Some(bs)) =>
        val triangles = ts.iterator.toSeq ++ bs.iterator.toSeq
        new OctreeBroadphase(triangles.iterator, pos * 16 + Repeated(8), 64)
      case _ => EmptyBroadphase
    }
  })

  def putEntity(entity: Entity): Chunk =
    new Chunk(pos, terrain, entities + (entity.id -> entity), terrainSoup, blockSoup, broadphase, terrainMesher.orNull, blockMesher.orNull)

  def removeEntity(entity: UUID): Chunk =
    new Chunk(pos, terrain, entities - entity, terrainSoup, blockSoup, broadphase, terrainMesher.orNull, blockMesher.orNull)

  def setTerrain(neu: Terrain): Chunk =
    new Chunk(pos, neu, entities, None, None, lastBroadphase, terrainMesher.orNull, blockMesher.orNull)

  def setTerrainSoup(ts: TerrainSoup): Chunk =
    new Chunk(pos, terrain, entities, Some(ts), blockSoup, null, null, blockMesher.orNull)

  def setBlockSoup(bs: BlockSoup): Chunk =
    new Chunk(pos, terrain, entities, terrainSoup, Some(bs), null, terrainMesher.orNull, null)

  def invalidate: Chunk =
    new Chunk(pos, terrain, entities, None, None, broadphase, terrainMesher.orNull, blockMesher.orNull, false, false)

  def isComplete: Boolean =
    terrainSoup.isDefined && blockSoup.isDefined

  def isActive: Boolean =
    entities nonEmpty

  def makeComplete(world: World): Chunk = {
    val ts = TerrainSoup(terrain, world).get
    val bs = BlockSoup(terrain, world).get
    new Chunk(pos, terrain, entities, Some(ts), Some(bs), null, null, null)
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