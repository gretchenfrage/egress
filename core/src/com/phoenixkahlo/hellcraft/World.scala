package com.phoenixkahlo.hellcraft

import java.util.UUID

import com.phoenixkahlo.hellcraft.util.{Directions, V3I, West}

/**
  * Minimal interface to world state
  */
trait World {

  def blockAt(v: V3I): Option[Block]

  def findEntity(id: UUID): Entity

}

object World {

  def putBlock(v: V3I, block: Block): Seq[ChunkEvent] =
    ChunkEvent(v / 16 floor, _.putBlock(v % 16, block)) +:
      Directions().map(d =>
        if ((v / 16 floor) != ((v + d) / 16 floor)) Some(ChunkEvent((v + d) / 16 floor, _.renderUncached))
        else None
      ).filter(_ isDefined).map(_.get)

}