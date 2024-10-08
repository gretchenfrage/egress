package com.phoenixkahlo.hellcraft.singleplayer

import java.io.{FileInputStream, FileOutputStream, Serializable}
import java.nio.file.Path

import com.phoenixkahlo.hellcraft.core.{Chunk, Terrain}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.singleplayer.AsyncSave.{DataKey, GetPos}
import com.phoenixkahlo.hellcraft.util.threading.{Fut, FutSequence, Promise, UniExecutor}

import scala.collection.mutable



trait AsyncSave[T <: GetPos with Serializable] {
  def push(chunks: Map[V3I, T]): Promise

  def push(chunks: Seq[T]): Promise =
    push(chunks.map(c => (c.pos, c)).toMap)

  def pull(chunks: Seq[V3I], terrain: Seq[V3I]): (Map[V3I, Fut[T]], Map[V3I, Fut[Terrain]])

  def putKey[K](key: DataKey[K], value: K): Promise

  def getKey[K](key: DataKey[K]): Fut[K]

  def close(chunks: Map[V3I, T]): Promise
}

object AsyncSave {
  trait GetPos {
    def pos: V3I
  }

  trait DataKey[T] {
    def code: Long
    def default: T
  }
}