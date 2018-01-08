package com.phoenixkahlo.hellcraft.core.event

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.{EntID, Entity, Moveable, PhysCube}
import com.phoenixkahlo.hellcraft.core.eval.WEval
import com.phoenixkahlo.hellcraft.core.request.{Request, Requested}
import com.phoenixkahlo.hellcraft.core.{Chunk, Event, MakeRequest, PutChunk, PutEnt, Terrain, TerrainUnit, UpdateEffect, UpdateEffectType}
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}


case object Events {
  def fulfill(p: V3I, requested: Requested): Event =
    Event(UE.chunk(p).map({
      case Some(chunk) => Seq(PutChunk(chunk.fulfill(requested)))
      case None => Seq(MakeRequest(Request(WEval.chunk(p)), requested => Seq(fulfill(p, requested))))
    }))

  def invalidate(p: V3I, mtf: Boolean = false, mbf: Boolean = false): Event =
    Event(UE.chunk(p).map({
      case Some(chunk) =>
        val (c, e) = chunk.invalidate(mtf, mbf)
        PutChunk(c) +: e
      case None => Seq(MakeRequest(Request(WEval.chunk(p)), requested => Seq(invalidate(p, mtf, mbf))))
    }))

  def setMat(v: V3I, mat: TerrainUnit, mtf: Boolean = false, mbf: Boolean = false): Event = Event({
    val p = v / 16 floor
    val surrounding = (p.neighbors.map(_ / 16 floor).toSet - p).toSeq
    UE.chunk(p).map({
      case Some(chunk) =>
        val (c, e) = chunk.setTerrain(Terrain(chunk.pos, chunk.terrain.grid.updated(v % 16, mat)), mtf, mbf)
        PutChunk(c) +: e ++: surrounding.map(invalidate(_, mtf, mbf))
      case None => Seq(MakeRequest(Request(WEval.chunk(p)), requested => Seq(setMat(v, mat, mtf, mbf))))
    })
  })

  def upd8ent[E <: Entity[E]](id: EntID[E])(func: E => E) =
    Event(UE.ent(id).map(_.map(ent => Seq(PutEnt(func(ent)))).getOrElse(Seq.empty)))

  def shift[E <: Moveable[E]](entID: EntID[E], dx: V3F) =
    upd8ent(entID)(ent => ent.updatePos(ent.pos + dx))

  /*
  def shift[E <: Moveable[E]](entID: EntID[E], dx: V3F) =
    Event(UE.ent(entID).map(ent => Seq(PutEnt(ent.updatePos(ent.pos + dx)))))

  def physics(entID: EntID[PhysCube]) =
    Event(UE.ent())
    */

}