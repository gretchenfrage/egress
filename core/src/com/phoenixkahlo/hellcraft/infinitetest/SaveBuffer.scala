package com.phoenixkahlo.hellcraft.infinitetest

import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.save.WorldSave
import com.phoenixkahlo.hellcraft.util.Scheduler

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

/*
class SaveBuffer(val wraps: WorldSave) {

  private val buffer: mutable.Map[V3I, Either[Chunk, Future[Chunk]]] = new mutable.HashMap

  def push(chunks: Seq[Chunk]): Unit = {
    buffer.synchronized { for (chunk <- chunks) buffer.put(chunk.pos, Left(chunk)) }
    // but what if it's loaded back in?
    Scheduler.schedule(() => buffer.synchronized { for (chunk <- chunks) buffer.remove(chunk.pos) }, 1 minutes)
  }

  def pull(ps: Seq[V3I]): Map[V3I, Chunk] = {

  }

  def buffer(loaded: Seq[V3I], thickness: Int) = {

  }

}
*/