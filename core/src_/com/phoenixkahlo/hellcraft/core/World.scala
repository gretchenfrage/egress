package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.math.{Directions, V3I}

/**
  * Minimal interface to world state
  */
trait World {

  def chunkAt(chunkPos: V3I): Option[Chunk]

  def chunkIsDefinedAt(chunkPos: V3I): Boolean

  def blockAt(v: V3I): Option[Block] = chunkAt(v / 16 floor).flatMap(_ (v % 16))

  def findEntity(id: UUID): Option[Entity]

  def time: Long

}

object World {

  def putBlock(v: V3I, block: Block, ids: Stream[UUID]): Seq[ChunkEvent] =
    PutBlock(v, block, ids.head) +:
      Directions().zip(ids.drop(1)).flatMap({ case (d, id) =>
        if ((v / 16 floor) != ((v + d) / 16 floor)) Some(UncacheMesh((v + d) / 16 floor, id))
        else None
      })

}