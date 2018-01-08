package com.phoenixkahlo.hellcraft.core

import java.util.{Objects, UUID}

import com.badlogic.gdx.graphics.g2d.TextureRegion
import com.phoenixkahlo.hellcraft.core.entity._
import com.phoenixkahlo.hellcraft.core.eval.GEval.GEval
import com.phoenixkahlo.hellcraft.core.eval.WEval.WEval
import com.phoenixkahlo.hellcraft.core.eval.{Exec3D, ExecCheap, GEval, WEval}
import com.phoenixkahlo.hellcraft.core.event.{Events, UEContext}
import com.phoenixkahlo.hellcraft.core.graphics.RenderWorld
import com.phoenixkahlo.hellcraft.core.request._
import com.phoenixkahlo.hellcraft.core.util.Derived
import com.phoenixkahlo.hellcraft.fgraphics._
import com.phoenixkahlo.hellcraft.math.physics._
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.LeftOption
import com.phoenixkahlo.hellcraft.util.collections.{Lazy, MemoFunc}
import com.phoenixkahlo.hellcraft.util.fields.{ByteFractionField, ByteFractionFieldBuffer, OptionField}

import scala.collection.mutable.ArrayBuffer

class Chunk(
             val pos: V3I,
             val terrain: Terrain,
             val terrainSoup: TerrainSoup,
             val blockSoup: BlockSoup,
             val tsRequest: Option[Request[TerrainSoup]] = None,
             val bsRequest: Option[Request[BlockSoup]] = None,
             val _broadphase: Derived[Broadphase] = new Derived[Broadphase],
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

  def broadphase: Broadphase =
    _broadphase(new OctreeBroadphase(terrainSoup.iterator ++ blockSoup.iterator, pos * 16 + Repeated(8), 64))

  /*
  @transient lazy val broadphase: Lazy[Broadphase] =
    Option(lastBroadphase).getOrElse(Lazy)
    */
    /*
    Option(lastBroadphase).map(_ apply).getOrElse(new OctreeBroadphase(
      terrainSoup.iterator ++ blockSoup.iterator,
      pos * 16 + Repeated(8), 64
    ))
    */

  /*
  def putEntity(entity: AnyEnt): Chunk =
    new Chunk(
      pos, terrain,
      entities + entity,
      terrainSoup, blockSoup,
      tsRequest, bsRequest,
      () => broadphase,
      Some(terrainRenderable), true, Some(blockRenderable), true
    )

  def removeEntity(entity: AnyEntID): Chunk =
    new Chunk(
      pos, terrain,
      entities - entity,
      terrainSoup, blockSoup,
      tsRequest, bsRequest,
      () => broadphase,
      Some(terrainRenderable), true, Some(blockRenderable), true
    )
    */

  def setTerrain(newTerrain: Terrain, meshTerrFast: Boolean = false, meshBlocksFast: Boolean = true): (Chunk, Seq[UpdateEffect]) =
    new Chunk(
      pos, newTerrain,
      terrainSoup, blockSoup,
      tsRequest, bsRequest,
      _broadphase,
      Some(terrainRenderable), true,
      Some(blockRenderable), true
    ).invalidate(meshTerrFast, meshBlocksFast)

  def invalidate(meshTerrFast: Boolean = false, meshBlocksFast: Boolean = true): (Chunk, Seq[UpdateEffect]) = {
    val e3d = Exec3D(pos * 16)
    val emicroworld = WEval.terrains(pos.neighbors)
    val ets: WEval[TerrainSoup] = emicroworld.map(world => TerrainSoup(terrain, world).get)(
      if (meshTerrFast) ExecCheap else e3d
    )
    val ebs: WEval[BlockSoup] = emicroworld.map(world => BlockSoup(terrain, world).get)(
      if (meshBlocksFast) ExecCheap else e3d
    )
    val rts: Request[TerrainSoup] = Request(ets, UEContext.randUUID())
    val rbs: Request[BlockSoup] = Request(ebs, UEContext.randUUID())
    new Chunk(
      pos, terrain,
      terrainSoup, blockSoup,
      Some(rts), Some(rbs),
      _broadphase,
      Some(terrainRenderable), true,
      Some(blockRenderable), true
    ) -> Seq(
      MakeRequest(rts, requested => Seq(Events.fulfill(pos, requested))),
      MakeRequest(rbs, requested => Seq(Events.fulfill(pos, requested)))
    )
  }

  def fulfill(requested: Requested): Chunk = {
    if (tsRequest.isDefined && tsRequest.get.unlock(requested).isDefined) {
      new Chunk(
        pos, terrain,
        tsRequest.get.unlock(requested).get, blockSoup,
        None, bsRequest,
        new Derived[Broadphase],
        Some(terrainRenderable), false,
        Some(blockRenderable), true
      )
    } else if (bsRequest.isDefined && bsRequest.get.unlock(requested).isDefined) {
      new Chunk(
        pos, terrain,
        terrainSoup, bsRequest.get.unlock(requested).get,
        tsRequest, None,
        new Derived[Broadphase],
        Some(terrainRenderable), true,
        Some(blockRenderable), false
      )
    } else this
  }

  def isRenderable: Boolean = terrainSoup.verts.nonEmpty || blockSoup.verts.nonEmpty

  def render(world: RenderWorld): Seq[Render[_ <: Shader]] = {
    Seq(
      Render[TerrainShader](terrainRenderable, Offset.default),
      Render[GenericShader](blockRenderable, Offset.default)
    )
  }

}
