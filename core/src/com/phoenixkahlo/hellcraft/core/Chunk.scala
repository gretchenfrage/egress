package com.phoenixkahlo.hellcraft.core

import java.util.{Objects, UUID}

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.core.request._
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.math.physics._
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.LeftOption
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc
import com.phoenixkahlo.hellcraft.util.fields.{ByteFractionField, ByteFractionFieldBuffer, OptionField}

class Chunk(
             val pos: V3I,
             val terrain: Terrain,
             val entities: Map[UUID, Entity],
             val terrainSoup: TerrainSoup,
             val blockSoup: BlockSoup,
             val tsRequest: Option[Request[TerrainSoup]],
             val bsRequest: Option[Request[BlockSoup]],
             @transient lastBroadphase: () => Broadphase = null,
             @transient lastTerrainMesher: () => TerrainMesher = null,
             @transient lastBlockMesher: () => BlockMesher = null
           ) extends Serializable {

  @transient lazy val terrainMesher: TerrainMesher =
    Option(lastTerrainMesher).map(_ ()).getOrElse(new TerrainMesher(this, terrainSoup))

  @transient lazy val blockMesher: BlockMesher =
    Option(lastBlockMesher).map(_ ()).getOrElse(new BlockMesher(this, blockSoup))

  @transient lazy val broadphase: Broadphase =
    Option(lastBroadphase).map(_ ()).getOrElse(new OctreeBroadphase(
      terrainSoup.iterator ++ blockSoup.iterator,
      pos * 16 + Repeated(8), 64
    ))

  def putEntity(entity: Entity): Chunk =
    new Chunk(
      pos, terrain,
      entities + (entity.id -> entity),
      terrainSoup, blockSoup,
      tsRequest, bsRequest,
      () => broadphase,
      () => terrainMesher, () => blockMesher
    )

  def removeEntity(entity: UUID): Chunk =
    new Chunk(
      pos, terrain,
      entities - entity,
      terrainSoup, blockSoup,
      tsRequest, bsRequest,
      () => broadphase,
      () => terrainMesher, () => blockMesher
    )

  def setTerrain(newTerrain: Terrain, ids: Stream[UUID]): (Chunk, Seq[UpdateEffect]) =
    new Chunk(
      pos, newTerrain,
      entities,
      terrainSoup, blockSoup,
      tsRequest, bsRequest,
      () => broadphase,
      () => terrainMesher,
      () => blockMesher
    ).invalidate(ids)

  def invalidate(ids: Stream[UUID]): (Chunk, Seq[UpdateEffect]) = {
    implicit val exec = Exec3D(pos * 16)

    val emicroworld: Evalable[TerrainGrid] =
      pos.neighbors
        .map(Evalable.chunk)
        .foldLeft(Evalable(Map.empty[V3I, Chunk])(ExecCheap))(
          (emap: Evalable[Map[V3I, Chunk]], echunk: Evalable[Chunk]) =>
            Evalable.merge(emap, echunk,
              (map: Map[V3I, Chunk], chunk: Chunk) => map + (chunk.pos -> chunk)
            )(ExecCheap)
        )
        .map(chunks => {
          new TerrainGrid {
            override def terrainAt(p: V3I): Option[Terrain] = chunks.get(p).map(_.terrain)
          }
          /*
          // TODO: this is hacky
          new World {
            override def boundingBox: (V3I, V3I) = ???
            override def res: Int = 16
            override def chunkAt(p: V3I): Option[Chunk] = chunks.get(p)
            override def debugChunkMap: Map[V3I, Chunk] = ???
            override def findEntity(id: UUID): Option[Entity] = ???
            override def time: Long = ???
          }
          */
        })
    val ets: Evalable[TerrainSoup] = emicroworld.map(world => TerrainSoup(terrain, world).get)
    val ebs: Evalable[BlockSoup] = emicroworld.map(world => BlockSoup(terrain, world).get)
    val rts: Request[TerrainSoup] = Request(ets, ids.drop(0).head)
    val rbs: Request[BlockSoup] = Request(ebs, ids.drop(1).head)
    new Chunk(
      pos, terrain,
      entities,
      terrainSoup, blockSoup,
      Some(rts), Some(rbs),
      () => broadphase,
      () => terrainMesher,
      () => blockMesher
    ) -> Seq(
      MakeRequest(rts, (requested, world) => Seq(FulfillChunk(pos, requested, ids.drop(2).head))),
      MakeRequest(rbs, (requested, world) => Seq(FulfillChunk(pos, requested, ids.drop(3).head)))
    )
  }

  def fulfill(requested: Requested): Chunk = {
    if (tsRequest.isDefined && tsRequest.get.unlock(requested).isDefined) {
      new Chunk(
        pos, terrain,
        entities,
        tsRequest.get.unlock(requested).get, blockSoup,
        None, bsRequest,
        () => broadphase,
        () => terrainMesher,
        () => blockMesher
      )
    } else if (bsRequest.isDefined && bsRequest.get.unlock(requested).isDefined) {
      new Chunk(
        pos, terrain,
        entities,
        terrainSoup, bsRequest.get.unlock(requested).get,
        tsRequest, None,
        () => broadphase,
        () => terrainMesher,
        () => blockMesher
      )
    } else this
  }

  def update(world: World): Seq[UpdateEffect] = {
    val seed: Long = (world.time.hashCode().toLong << 32) | pos.hashCode().toLong
    val idss: Stream[Stream[UUID]] = RNG.meta(RNG(seed), RNG.uuids)
    entities.values.zip(idss).flatMap({ case (entity, ids) => entity.update(world, ids) }).toSeq
  }

  def isActive: Boolean = entities.nonEmpty

  def renderables(pack: ResourcePack, world: World): Seq[RenderUnit] = {
    entities.values.flatMap(_.renderables(pack)).toSeq ++ terrainMesher(world, pack) ++ blockMesher(world, pack)
  }
}
