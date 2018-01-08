package com.phoenixkahlo.hellcraft.core.event

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.{EntID, Entity, Moveable, PhysCube}
import com.phoenixkahlo.hellcraft.core.request.Requested
import com.phoenixkahlo.hellcraft.core.{Event, PutChunk, PutEnt, Terrain, TerrainUnit, UpdateEffect, UpdateEffectType}
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}


case object Events {
  def fulfill(p: V3I, requested: Requested) =
    Event(UE.chunk(p).map({
      case Some(chunk) => Seq(PutChunk(chunk.fulfill(requested)))
      case None => ??? // TODO: request asynchronously
    }))

  def invalidate(p: V3I, mtf: Boolean = false, mbf: Boolean = false) =
    Event(UE.chunk(p).map({
      case Some(chunk) =>
        val (c, e) = chunk.invalidate(mtf, mbf)
        PutChunk(c) +: e
      case None => ??? // TODO: make request to invalidate asynchronously
    }))

  def setMat(v: V3I, mat: TerrainUnit, mtf: Boolean = false, mbf: Boolean = false) = Event({
    val p = v / 16 floor
    val surrounding = (p.neighbors.map(_ / 16 floor).toSet - p).toSeq
    UE.chunk(p).map({
      case Some(chunk) =>
        val (c, e) = chunk.setTerrain(Terrain(chunk.pos, chunk.terrain.grid.updated(v % 16, mat)), mtf, mbf)
        PutChunk(c) +: e ++: surrounding.map(invalidate(_, mtf, mbf))
      case None => ??? // TODO: request asynchronously
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