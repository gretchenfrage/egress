package com.phoenixkahlo.hellcraft.core

import java.util.{Objects, UUID}

import com.badlogic.gdx.graphics.g2d.TextureRegion
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.core.eval.GEval.GEval
import com.phoenixkahlo.hellcraft.core.eval.WEval.WEval
import com.phoenixkahlo.hellcraft.core.eval.{Exec3D, ExecCheap, GEval, WEval}
import com.phoenixkahlo.hellcraft.core.request._
import com.phoenixkahlo.hellcraft.fgraphics._
import com.phoenixkahlo.hellcraft.math.physics._
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.LeftOption
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc
import com.phoenixkahlo.hellcraft.util.fields.{ByteFractionField, ByteFractionFieldBuffer, OptionField}

import scala.collection.mutable.ArrayBuffer

class Chunk(
             val pos: V3I,
             val terrain: Terrain,
             val entities: Map[UUID, Entity],
             val terrainSoup: TerrainSoup,
             val blockSoup: BlockSoup,
             val tsRequest: Option[Request[TerrainSoup]],
             val bsRequest: Option[Request[BlockSoup]],
             @transient lastBroadphase: () => Broadphase = null,
             lastTerrainRenderable: Option[Renderable[TerrainShader]] = None,
             lastTerrainRenderableValid: Boolean = false,
             lastBlockRenderable: Option[Renderable[GenericShader]] = None,
             lastBlockRenderableValid: Boolean = false
           ) extends Serializable {

  implicit val exec = Exec3D(pos * 16)

  val terrainRenderable: Renderable[TerrainShader] = {
    def gen: GEval[TerrainShader#RenderUnit] = GEval.resourcePack.map(pack => {
      val verts = new ArrayBuffer[BasicTriVert]
      for (v: V3I <- terrainSoup.indexToVert) {
        val vert: TerrainSoup.Vert = terrainSoup.verts(v).get
        val tex: TextureRegion = pack(vert.mat.tid)
        verts += BasicTriVert(vert.pos, V4I.ones, V2F(tex.getU, tex.getV), vert.nor)
      }
      (verts, terrainSoup.indices)
    })

    lastTerrainRenderable match {
      case Some(ltr) if lastTerrainRenderableValid => ltr
      case Some(ltr) => ltr.update(gen)
      case None => Renderable[TerrainShader](gen)
    }
  }

  val blockRenderable: Renderable[GenericShader] = {
    def gen: GEval[GenericShader#RenderUnit] = GEval.resourcePack.map(pack => {
      val verts = new ArrayBuffer[BasicTriVert]
      for (vert <- blockSoup.verts) {
        val tex = pack(vert.block.tid)
        verts += BasicTriVert(vert.pos, V4I.ones, V2F(
          tex.getU + (vert.uvDelta.x / 16f), tex.getV + (vert.uvDelta.y / 16f)
        ), vert.nor)
      }
      (verts, blockSoup.indices)
    })

    lastBlockRenderable match {
      case Some(lbr) if lastBlockRenderableValid => lbr
      case Some(lbr) => lbr.update(gen)
      case None => Renderable[GenericShader](gen)
    }
  }

  @transient lazy val broadphase: Broadphase =
    Option(lastBroadphase).map(_ apply).getOrElse(new OctreeBroadphase(
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
      Some(terrainRenderable), true, Some(blockRenderable), true
    )

  def removeEntity(entity: UUID): Chunk =
    new Chunk(
      pos, terrain,
      entities - entity,
      terrainSoup, blockSoup,
      tsRequest, bsRequest,
      () => broadphase,
      Some(terrainRenderable), true, Some(blockRenderable), true
    )

  def setTerrain(newTerrain: Terrain, meshTerrFast: Boolean = false, meshBlocksFast: Boolean = true)(implicit rand: MRNG): (Chunk, Seq[UpdateEffect]) =
    new Chunk(
      pos, newTerrain,
      entities,
      terrainSoup, blockSoup,
      tsRequest, bsRequest,
      () => broadphase,
      Some(terrainRenderable), true,
      Some(blockRenderable), true
    ).invalidate(meshTerrFast, meshBlocksFast)

  def invalidate(meshTerrFast: Boolean = false, meshBlocksFast: Boolean = true)(implicit rand: MRNG): (Chunk, Seq[UpdateEffect]) = {
    val e3d = Exec3D(pos * 16)
    val emicroworld = WEval.terrains(pos.neighbors)
    val ets: WEval[TerrainSoup] = emicroworld.map(world => TerrainSoup(terrain, world).get)(
      if (meshTerrFast) ExecCheap else e3d
    )
    val ebs: WEval[BlockSoup] = emicroworld.map(world => BlockSoup(terrain, world).get)(
      if (meshBlocksFast) ExecCheap else e3d
    )
    val rts: Request[TerrainSoup] = Request(ets, rand.nextUUID)
    val rbs: Request[BlockSoup] = Request(ebs, rand.nextUUID)
    new Chunk(
      pos, terrain,
      entities,
      terrainSoup, blockSoup,
      Some(rts), Some(rbs),
      () => broadphase,
      Some(terrainRenderable), true,
      Some(blockRenderable), true
    ) -> Seq(
      MakeRequest(rts, (requested, world) => Seq(ChunkEvent.fulfill(pos, requested))),
      MakeRequest(rbs, (requested, world) => Seq(ChunkEvent.fulfill(pos, requested)))
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
        Some(terrainRenderable), false,
        Some(blockRenderable), true
      )
    } else if (bsRequest.isDefined && bsRequest.get.unlock(requested).isDefined) {
      new Chunk(
        pos, terrain,
        entities,
        terrainSoup, bsRequest.get.unlock(requested).get,
        tsRequest, None,
        () => broadphase,
        Some(terrainRenderable), true,
        Some(blockRenderable), false
      )
    } else this
  }

  def update(world: World)(implicit rand: MRNG): Seq[UpdateEffect] = {
    //val seed: Long = (world.time.hashCode().toLong << 32) | pos.hashCode().toLong
    //val idss: Stream[Stream[UUID]] = RNG.meta(RNG(seed), RNG.uuids)
    //entities.values.zip(idss).flatMap({ case (entity, ids) => entity.update(world, ids) }).toSeq
    entities.values.toSeq.flatMap(_.update(world))
  }

  def isActive: Boolean = entities.nonEmpty

  def isRenderable: Boolean = entities.nonEmpty || terrainSoup.verts.nonEmpty || blockSoup.verts.nonEmpty

  def render(world: RenderWorld): Seq[Render[_ <: Shader]] = {
    Seq(
      Render[TerrainShader](terrainRenderable, Offset.default),
      Render[GenericShader](blockRenderable, Offset.default)
    ) ++ entities.values.flatMap(_.render(world))
  }

}
