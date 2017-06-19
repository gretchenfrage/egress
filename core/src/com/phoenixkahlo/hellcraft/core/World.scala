package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.math.{Directions, V3I}

/**
  * Minimal interface to world state
  */
trait World {

  def blockAt(v: V3I): Option[Block]

  def findEntity(id: UUID): Entity

  def chunkAt(chunkPos: V3I): Option[Chunk]

}

object World {

  def putBlock(v: V3I, block: Block): Seq[ChunkEvent] =
    ChunkEvent(v / 16 floor, _.putBlock(v % 16, block)) +:
      Directions().map(d =>
        if ((v / 16 floor) != ((v + d) / 16 floor)) Some(ChunkEvent((v + d) / 16 floor, _.renderUncached))
        else None
      ).filter(_ isDefined).map(_.get)

}