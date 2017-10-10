package com.phoenixkahlo.hellcraft.core

import java.util.{Objects, UUID}

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.graphics.{ChunkMesher, RenderUnit, ResourcePack}
import com.phoenixkahlo.hellcraft.graphics.RenderUnit
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.fields.{ByteFractionField, ByteFractionFieldBuffer, FloatField, OptionField}

@CarboniteWith(classOf[FieldNode])
class Chunk(
             val pos: V3I,
             val terrain: Terrain,
             val entities: Map[UUID, Entity] = Map.empty,
             val polarity: Option[FloatField] = None,
             @transient lastMesher: ChunkMesher = null,
             @transient lastMesherValid: Boolean = false
           ) {

  /*
  @transient lazy val mesher: Option[ChunkMesher] = Option(lastMesher) match {
    case last if last isDefined => last
    case None => terrain.asMeshable match {
      case Some(meshable) => Some(new ChunkMesher(this, meshable))
      case None => None
    }
  }
  */
  @transient lazy val mesher: Option[ChunkMesher] = Option(lastMesher) match {
    case last if last.isDefined && lastMesherValid => last
    case last => terrain.asMeshable match {
      case Some(meshable) => Some(new ChunkMesher(this, meshable))
      case None if last.isDefined => last
      case None => None
    }
  }

  def putEntity(entity: Entity): Chunk =
    new Chunk(pos, terrain, entities + (entity.id -> entity), polarity, mesher.orNull, true)

  def removeEntity(entity: UUID): Chunk =
    new Chunk(pos, terrain, entities - entity, polarity, mesher.orNull, true)

  def updateTerrain(neu: Terrain): Chunk =
    new Chunk(pos, neu, entities, polarity, mesher.orNull, false)

  def updatePolarity(neu: Option[FloatField]): Chunk =
    new Chunk(pos, terrain, entities, neu, mesher.orNull, true)

  def computePolarity: Chunk = {
    updatePolarity(Some(FloatField(terrain.densities.size, v => {
      val density = terrain.densities(v).get

      if (density < 0.5) -density
      else density
    })))
  }

  def dropPolarity: Chunk =
    updatePolarity(None)

  def flow(world: World): Chunk =
    polarity match {
      case None => this
      case Some(polarity) =>
        updateTerrain(Densities(pos, terrain.materials, FloatField(terrain.densities.size, v => {
          val vg = pos * world.res + v
          val p = polarity(v).get
          terrain.densities(v).get + Directions().flatMap(d => {
            world.chunkAt((vg + d) / world.res floor).flatMap(_.polarity).map(p - _.atMod(vg))
          }).sum
        }))).dropPolarity
    }

  def update(world: World): Seq[UpdateEffect] = {
    val seed: Long = (world.time.hashCode().toLong << 32) | pos.hashCode().toLong
    val idss: Stream[Stream[UUID]] = RNG.meta(RNG(seed), RNG.uuids)
    var effects = entities.values.zip(idss.drop(1)).flatMap({ case (entity, ids) => entity.update(world, ids) }).toSeq
    if (world.time % 20 == 0) {
      effects +:= Polarize(pos, idss.head.head)
    } else if (world.time % 20 == 10) {
      effects +:= Flow(pos, idss.head.head)
    }
    effects
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