package com.phoenixkahlo.hellcraft.core.event

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.{EntID, Entity, Moveable, PhysCube}
import com.phoenixkahlo.hellcraft.core.eval.WEval
import com.phoenixkahlo.hellcraft.core.request.{Request, Requested}
import com.phoenixkahlo.hellcraft.core.{Chunk, Event, MakeRequest, PutChunk, PutEnt, Terrain, TerrainUnit, UpdateEffect, UpdateEffectType}
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}


case object Events {
  def fulfill(p: V3I, requested: Requested): Event =
    Delayable(DE.chunk(p).map(chunk => Seq(PutChunk(chunk.fulfill(requested)))))

  def invalidate(p: V3I, mtf: Boolean = false, mbf: Boolean = false): Event =
    Delayable(for {
      chunk <- DE.chunk(p)
      rand <- DE.rand
    } yield {
      val (c, e) = chunk.invalidate(mtf, mbf)(rand)
      PutChunk(c) +: e
    })

  def setMat(v: V3I, mat: TerrainUnit, mtf: Boolean = false, mbf: Boolean = false): Event = {
    val p = v / 16 floor
    val surrounding = (v.neighbors.map(_ / 16 floor).toSet - p).toSeq
    Delayable(for {
      chunk <- DE.chunk(p)
      rand <- DE.rand
    } yield {
      val (c, e) = chunk.setTerrain(Terrain(chunk.pos, chunk.terrain.grid.updated(v % 16, mat)), mtf, mbf)(rand)
      PutChunk(c) +: e ++: surrounding.map(invalidate(_, mtf, mbf))
    })
  }

  def upd8ent[E <: Entity[E]](id: EntID[E])(func: E => E) =
    Event(UE.ent(id).map(_.map(ent => Seq(PutEnt(func(ent)))).getOrElse(Seq.empty)))

  def shift[E <: Moveable[E]](entID: EntID[E], dx: V3F) =
    upd8ent(entID)(ent => ent.updatePos(ent.pos + dx))

}