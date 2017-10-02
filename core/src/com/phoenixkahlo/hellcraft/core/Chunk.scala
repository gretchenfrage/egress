package com.phoenixkahlo.hellcraft.core

import java.util.{Objects, UUID}

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.graphics.{ChunkMesher, RenderUnit, ResourcePack}
import com.phoenixkahlo.hellcraft.graphics.RenderUnit
import com.phoenixkahlo.hellcraft.math.{RNG, V3F, V3I}
import com.phoenixkahlo.hellcraft.util.fields.{FractionField, OptionField}

@CarboniteWith(classOf[FieldNode])
class Chunk(
           val pos: V3I,
           val terrain: Terrain,
           val entities: Map[UUID, Entity] = Map.empty,
           @transient lastMesher: ChunkMesher = null
           ) {

  @transient lazy val mesher: Option[ChunkMesher] = Option(lastMesher) match {
    case last if last isDefined => last
    case None => terrain.asMeshable match {
      case Some(meshable) => Some(new ChunkMesher(this, meshable))
      case None => None
    }
  }

  def putEntity(entity: Entity): Chunk =
    new Chunk(pos, terrain, entities + (entity.id -> entity), mesher.orNull)

  def removeEntity(entity: UUID): Chunk =
    new Chunk(pos, terrain, entities - entity, mesher.orNull)

  def updateTerrain(neu: Terrain): Chunk =
    new Chunk(pos, neu, entities, null)

  def update(world: World, dt: Float): Seq[UpdateEffect] = {
    val seed: Long = (world.time.hashCode().toLong << 32) | pos.hashCode().toLong
    val idss: Stream[Stream[UUID]] = RNG.meta(RNG(seed), RNG.uuids)
    entities.values.zip(idss).flatMap({ case (entity, ids) => entity.update(world, ids, dt) }).toSeq
  }

  override def hashCode(): Int =
    Objects.hash(pos, terrain, entities)

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case chunk: Chunk => pos == chunk.pos && terrain == chunk.terrain && entities == chunk.entities
      case _ => false
    }

  def renderables(pack: ResourcePack, world: World): Seq[RenderUnit] = {
    entities.values.flatMap(_.renderables(pack)).toSeq ++ mesher.map(_(world, pack)).getOrElse(Seq.empty)
  }

}