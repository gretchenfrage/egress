package com.phoenixkahlo.hellcraft.singleplayer

import java.io._
import java.nio.file.Path
import java.util.concurrent._

import com.esotericsoftware.kryo.io
import com.esotericsoftware.kryo.io.{Input, Output}
import com.phoenixkahlo.hellcraft.carbonite.egress.EgressCarboniteConfig
import com.phoenixkahlo.hellcraft.carbonite.{CarboniteInputStream, CarboniteOutputStream, DefaultCarboniteConfig, LazyDeserial}
import com.phoenixkahlo.hellcraft.oldcore.Chunk
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.threading.{Fut, FutSequences, UniExecutor}

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait AsyncSave {

  def push(chunks: Map[V3I, Chunk]): Seq[Fut[Unit]]

  def finalPush(chunks: Map[V3I, Chunk]): Seq[Fut[Unit]]

  def push(chunks: Seq[Chunk]): Seq[Fut[Unit]] =
    push(chunks.map(c => (c.pos, c)).toMap)

  def pull(chunks: Seq[V3I]): Map[V3I, Fut[Chunk]]

}

trait SaveSerialService {

  def write(path: Path, obj: Map[V3I, Chunk]): Unit

  def read(path: Path): Map[V3I, Fut[Chunk]]

}

class CarboniteSerialService extends SaveSerialService {
  private implicit val config = EgressCarboniteConfig

  override def write(path: Path, obj: Map[V3I, Chunk]): Unit = {
    val out = new CarboniteOutputStream(new FileOutputStream(path.toFile))
    out.writeObject(obj.mapValues(LazyDeserial(_)))
    out.close()
  }

  override def read(path: Path): Map[V3I, Fut[Chunk]] = {
    if (path.toFile.exists) {
      val in = new CarboniteInputStream(new FileInputStream(path.toFile))
      try in.readObject().asInstanceOf[Map[V3I, LazyDeserial[Chunk]]]
        .map({ case (p, laz) => (p, laz.fut(UniExecutor.exec(p * 16 + V3I(8, 8, 8)))) })
      finally in.close()
    } else Map.empty
  }
}

class RegionGenAsyncSave(path: Path, serial: SaveSerialService, generator: V3I => Fut[Chunk]) extends AsyncSave {

  if (!path.toFile.exists)
    path.toFile.mkdir()

  val RegionSize = 32

  private def pathFor(region: V3I): Path =
    path.resolve("x" + region.xi + "y" + region.yi + "z" + region.zi + ".region")

  private val regionSequences = new mutable.HashMap[V3I, FutSequences]
  private val regionSequenceLock = new Object

  private def sequenceFor(region: V3I): FutSequences = {
    regionSequenceLock.synchronized {
      regionSequences.get(region) match {
        case Some(factory) => factory
        case None =>
          val factory = new FutSequences(UniExecutor.exec)
          regionSequences.put(region, factory)
          factory
      }
    }
  }

  private def regionFut[T](region: V3I, factory: => T): Fut[T] = {
    sequenceFor(region)(factory)
  }

  def push(chunks: Map[V3I, Chunk], executor: Option[Runnable => Unit]): Seq[Fut[Unit]] = {
    val toSave = chunks.filter({ case (_, c) => !c.freshlyLoaded })
    if (toSave isEmpty) return Seq.empty

    var accumulator: Seq[Fut[Unit]] = Seq.empty
    for ((region, group) <- toSave.values.toSeq.groupBy(_.pos / RegionSize floor)) {
      accumulator +:= regionFut[Unit](region, {
        val path = pathFor(region)
        var map = serial.read(path).mapValues(_.await)
        map ++= group.map(c => (c.pos, c)).toMap
        serial.write(path, map)
      })
    }
    accumulator
  }

  override def push(chunks: Map[V3I, Chunk]): Seq[Fut[Unit]] = {
    push(chunks, None)
  }

  override def finalPush(chunks: Map[V3I, Chunk]): Seq[Fut[Unit]] = {
    push(chunks, Some(new Thread(_).start()))
  }

  override def pull(chunks: Seq[V3I]): Map[V3I, Fut[Chunk]] = {
    if (chunks isEmpty) return Map.empty

    var accumulator: Map[V3I, Fut[Chunk]] = Map.empty
    for ((region, group) <- chunks.groupBy(_ / RegionSize floor)) {
      val future: Fut[Map[V3I, Fut[Chunk]]] = regionFut(region, {
        val pulled = serial.read(pathFor(region))
        group.map(p => pulled.get(p) match {
          case Some(chunkFuture) => p -> chunkFuture
          case None => p -> generator(p)
        }).toMap
      })
      for (p <- group)
        accumulator = accumulator.updated(p, future.flatMap(_(p)))
    }
    accumulator
  }

}
