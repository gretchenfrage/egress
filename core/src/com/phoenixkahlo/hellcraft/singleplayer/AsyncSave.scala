package com.phoenixkahlo.hellcraft.singleplayer

import java.io.{FileInputStream, FileOutputStream}
import java.nio.file.Path

import com.phoenixkahlo.hellcraft.core.{Chunk, Terrain}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.threading.{Fut, FutSequences, Promise, UniExecutor}

import scala.collection.mutable

trait AsyncSave {
  def push(chunks: Map[V3I, Chunk]): Promise

  def push(chunks: Seq[Chunk]): Promise =
    push(chunks.map(c => (c.pos, c)).toMap)

  def pull(chunks: Seq[V3I], terrain: Seq[V3I]): (Map[V3I, Fut[Chunk]], Map[V3I, Fut[Terrain]])

  def close(chunks: Map[V3I, Chunk]): Promise
}

