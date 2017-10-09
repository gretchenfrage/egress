package com.phoenixkahlo.hellcraft.core

import java.util.{Objects, UUID}

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.graphics.{ChunkMesher, RenderUnit, ResourcePack}
import com.phoenixkahlo.hellcraft.graphics.RenderUnit
import com.phoenixkahlo.hellcraft.math.{Origin, RNG, V3F, V3I}
import com.phoenixkahlo.hellcraft.util.fields.{FractionField, FractionFieldBuffer, OptionField}

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
  /*
  def massFlow(world: World, ids: Stream[UUID]): Seq[ChunkEvent] = {
    /*
    val deltas: Map[V3I, FractionFieldBuffer] =
      (pos +: pos.touching).map(p => p -> new FractionFieldBuffer(world.resVec)).toMap

    def compDelta(v: V3I): Unit = {
      // density at terrain sample
      val density: Float = terrain.densities(v).get
      // if it wants to flow
      if (density > 0) {
        // material at terrain sample
        val mat: Material = Materials(terrain.materials(v).get)
        // global terrain sample coordinates
        val vg: V3I = pos * world.res + v
        // global terrain coordinates of where it can flow
        val targets: Seq[V3I] = vg.touching.filter(world.materialGridPoint(_).get == mat)
        // if it can flow anywhere
        if (targets nonEmpty) {
          // for each global terrain coordinate that it can flow into
          for (target: V3I <- targets) {
            // chunk coordinates of target
            val p = target / world.res floor
            // how much we will transfer
            val toTransfer: Float = Math.min(density / targets.size, 1 - (world.densityGridPoint(target).get + deltas(p)(target % world.res)))
          }
        }
      }
    }
    */

    for (v <- Origin until world.resVec) {
      compDelta(v)
    }

    /*
    var densities = Map.empty[V3I, FractionField].withDefault(_ => FractionField(world.resVec, _ => 0))
    for (v <- Origin until world.resVec) {
      val density = terrain.densities(v).get
      if (density > 0) {
        val vg = pos * world.res + v
        val mat = Materials(terrain.materials(v).get)
        val targets = vg.touching.filter(world.materialGridPoint(_).get == mat)
        if (targets nonEmpty) {
          for (target: V3I <- targets) {
            val p = target / world.res floor;
            densities = densities.updated(p, densities(p).updated(target % world.res, densities(p)(target % world.res) + ))
          }
        }
      }
    }
    */

    /*
    (Origin until world.resVec).foldLeft(zeroMap)({ case (deltas, v) => {
      val density: Float = terrain.densities(v).get
      if (density > 0) {
        val globalV: V3I = pos * world.res + v
        val material: Material = Materials(terrain.materials(v).get)
        val flowTargets: Seq[V3I] = globalV.touching.filter(world.materialGridPoint(_).get == material)
        if (flowTargets nonEmpty) {
          flowTargets.foldLeft

          ???
        } else deltas
      } else deltas
    }})
    */
  }
  */
  def update(world: World): Seq[UpdateEffect] = {
    val seed: Long = (world.time.hashCode().toLong << 32) | pos.hashCode().toLong
    val idss: Stream[Stream[UUID]] = RNG.meta(RNG(seed), RNG.uuids)
    Flow(pos, idss.head.head) +:
      entities.values.zip(idss.drop(1)).flatMap({ case (entity, ids) => entity.update(world, ids) }).toSeq
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