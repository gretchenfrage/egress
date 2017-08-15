package com.phoenixkahlo.hellcraft.singleplayer

import java.io._
import java.nio.file.Path
import java.util.concurrent.Executors

import com.esotericsoftware.kryo.io
import com.esotericsoftware.kryo.io.{Input, Output}
import com.phoenixkahlo.hellcraft.carbonite.egress.EgressCarboniteConfig
import com.phoenixkahlo.hellcraft.carbonite.{CarboniteInputStream, CarboniteOutputStream, DefaultCarboniteConfig, LazyDeserial}
import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.serial.GlobalKryo

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait AsyncSave extends AutoCloseable {

  def push(chunks: Map[V3I, Chunk]): Seq[Future[Unit]]

  def push(chunks: Seq[Chunk]): Seq[Future[Unit]] =
    push(chunks.map(c => (c.pos, c)).toMap)

  def pull(chunks: Seq[V3I]): Map[V3I, Future[Chunk]]

}

trait SaveSerialService {

  def write(path: Path, obj: Map[V3I, Chunk]): Unit

  def read(path: Path)(implicit executor: ExecutionContext): Map[V3I, Future[Chunk]]

}

class CarboniteSerialService extends SaveSerialService {
  private implicit val config = EgressCarboniteConfig

  /*
  override def write(path: Path, obj: Map[V3I, Chunk]): Unit = {
    val out = new CarboniteOutputStream(new FileOutputStream(path.toFile))
    out.writeObject(obj)
    out.close()
  }

  override def read(path: Path): Map[V3I, Future[Chunk]] = {
    if (path.toFile.exists) {
      val in = new CarboniteInputStream(new FileInputStream(path.toFile))
      try in.readObject().asInstanceOf[Map[V3I, Chunk]].mapValues(c => Future { c } (ExecutionContext.global))
      finally in.close()
    } else Map.empty
  }
  */
  override def write(path: Path, obj: Map[V3I, Chunk]): Unit = {
    val out = new CarboniteOutputStream(new FileOutputStream(path.toFile))
    out.writeObject(obj.mapValues(LazyDeserial(_)))
    out.close()
  }

  override def read(path: Path)(implicit executor: ExecutionContext): Map[V3I, Future[Chunk]] = {
    if (path.toFile.exists) {
      val in = new CarboniteInputStream(new FileInputStream(path.toFile))
      try in.readObject().asInstanceOf[Map[V3I, LazyDeserial[Chunk]]].mapValues(_.future)
      finally in.close()
    } else Map.empty
  }
}

class RegionGenAsyncSave(path: Path, serial: SaveSerialService, generator: V3I => Future[Chunk]) extends AsyncSave {

  if (!path.toFile.exists)
    path.toFile.mkdir()

  val RegionSize = 8

  private def pathFor(region: V3I): Path =
    path.resolve("x" + region.xi + "y" + region.yi + "z" + region.zi + ".region")

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
        val path = pathFor(region)
        var map = serial.read(path).mapValues(Await.result(_, Duration.Inf))
        map ++= group.map(c => (c.pos, c)).toMap
        serial.write(path, map)
      } (contextFor(region))
    }
    accumulator
  }

  override def pull(chunks: Seq[V3I]): Map[V3I, Future[Chunk]] = {
    if (chunks isEmpty) return Map.empty

    var accumulator: Map[V3I, Future[Chunk]] = Map.empty
    for ((region, group) <- chunks.groupBy(_ / RegionSize floor)) {
      val future: Future[Map[V3I, Future[Chunk]]] = Future {
        val pulled = serial.read(pathFor(region))
        group.map(p => pulled.get(p) match {
          case Some(chunkFuture) => p -> chunkFuture
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
