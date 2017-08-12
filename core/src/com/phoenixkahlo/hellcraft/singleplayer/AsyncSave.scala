package com.phoenixkahlo.hellcraft.singleplayer

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.file.Path
import java.util.concurrent.Executors

import com.esotericsoftware.kryo.io.{Input, Output}
import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.serial.GlobalKryo

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait AsyncSave extends AutoCloseable {

  def push(chunks: Map[V3I, Chunk]): Seq[Future[Unit]]

  def push(chunks: Seq[Chunk]): Seq[Future[Unit]] =
    push(chunks.map(c => (c.pos, c)).toMap)

  def pull(chunks: Seq[V3I]): Map[V3I, Future[Chunk]]

}

class RegionGenAsyncSave(path: Path, generator: V3I => Future[Chunk]) extends AsyncSave {

  if (!path.toFile.exists)
    path.toFile.mkdir()

  val RegionSize = 8

  private def file(region: V3I): File =
    path.resolve("x" + region.xi + "y" + region.yi + "z" + region.zi + ".region").toFile

  private val executors = new mutable.HashMap[V3I, ExecutionContext]
  private val executorLock = new Object
  private implicit val miscExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(
    Runtime.getRuntime.availableProcessors,
    runnable => {
      val thread = new Thread(runnable, "generator thread")
      thread.setPriority(3)
      thread
    }
  ))

  private def contextFor(region: V3I): ExecutionContext = {
    executorLock.synchronized {
      executors.get(region) match {
        case Some(executor) => executor
        case None =>
          val executor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(
            runnable => {
              val thread = new Thread(runnable, "region save/load thread")
              thread.setPriority(2)
              thread
            }
          ))
          executors.put(region, executor)
          executor
      }
    }
  }

  override def push(chunks: Map[V3I, Chunk]): Seq[Future[Unit]] = {
    val toSave = chunks.filter({ case (_, c) => !c.freshlyLoaded })
    if (toSave isEmpty) return Seq.empty

    var accumulator: Seq[Future[Unit]] = Seq.empty
    for ((region, group) <- toSave.values.toSeq.groupBy(_.pos / RegionSize floor)) {
      accumulator +:= Future {
        val regionFile = file(region)
        var map: Map[V3I, Chunk] =
          if (regionFile exists) {
            val in = new FileInputStream(file(region))
            val map = GlobalKryo().readObject(new Input(in), classOf[Map[V3I, Chunk]])
            in.close()
            map
          } else {
            regionFile.createNewFile()
            Map.empty
          }

        map ++= group.map(c => (c.pos, c)).toMap

        val out = new FileOutputStream(regionFile)
        GlobalKryo().writeObject(new Output(out), map)
        out.close()
      } (contextFor(region))
    }
    accumulator
  }

  override def pull(chunks: Seq[V3I]): Map[V3I, Future[Chunk]] = {
    if (chunks isEmpty) return Map.empty

    var accumulator: Map[V3I, Future[Chunk]] = Map.empty
    for ((region, group) <- chunks.groupBy(_ / RegionSize floor)) {
      val future: Future[Map[V3I, Future[Chunk]]] = Future {
        val regionFile = file(region)
        val pulled: Map[V3I, Chunk] =
          if (regionFile exists) {
            val in = new FileInputStream(regionFile)
            val pulled = GlobalKryo().readObject(new Input(in), classOf[Map[V3I, Chunk]])
            in.close()
            pulled
          } else Map.empty
        group.map(p => pulled.get(p) match {
          case Some(chunk) => p -> Future { chunk }
          case None => p -> generator(p)
        }).toMap
      }
      for (p <- group)
        accumulator = accumulator.updated(p, future.flatMap(_(p)))
    }
    accumulator
  }


  override def close(): Unit = {
    for (executor <- executors.values) {
      Await.result(Future { "done!" } (executor), Duration.Inf)
    }
  }

}
