package com.phoenixkahlo.hellcraft.singleplayer

import java.io._
import java.nio.file.Path
import java.util.concurrent.Executors

import com.esotericsoftware.kryo.io
import com.esotericsoftware.kryo.io.{Input, Output}
import com.phoenixkahlo.hellcraft.carbonite.egress.EgressCarboniteConfig
import com.phoenixkahlo.hellcraft.carbonite.{CarboniteInputStream, CarboniteOutputStream, DefaultCarboniteConfig}
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

  def read(path: Path): Map[V3I, Chunk]

}

class KryoSerialService extends SaveSerialService {
  val bufferSize = 1000000

  override def write(path: Path, obj: Map[V3I, Chunk]): Unit = {
    val file = path.toFile
    if (!file.exists) file.createNewFile()
    val out = new FileOutputStream(file)
    val output = new io.Output(out, bufferSize)
    val kryo = GlobalKryo()
    kryo.writeObject(output, obj)
    out.close()
  }

  override def read(path: Path): Map[V3I, Chunk] = {
    val file = path.toFile
    if (file exists) {
      val in = new FileInputStream(file)
      val input = new io.Input(in, bufferSize)
      val kryo = GlobalKryo()
      try kryo.readObject(input, classOf[Map[V3I, Chunk]])
      finally in.close()
    } else Map.empty
  }
}

class JavaSerialService extends SaveSerialService {
  override def write(path: Path, obj: Map[V3I, Chunk]): Unit = {
    val out = new ObjectOutputStream(new FileOutputStream(path.toFile))
    out.writeObject(obj)
    out.close()
  }

  override def read(path: Path): Map[V3I, Chunk] = {
    if (path.toFile.exists) {
      val in = new ObjectInputStream(new FileInputStream(path.toFile))
      try in.readObject().asInstanceOf[Map[V3I, Chunk]]
      finally in.close()
    } else Map.empty
  }
}

object ObjectOutputAsStream {
  def apply(oo: ObjectOutput): OutputStream = {
    oo match {
      case stream: OutputStream => stream
      case _ => b => oo.writeByte(b)
    }
  }
}

object ObjectInputAsStream {
  def apply(oi: ObjectInput): InputStream = {
    oi match {
      case stream: InputStream => stream
      case _ => () => oi.readByte()
    }
  }
}

class HybridChunkWrapper extends Externalizable {

  var chunk: Chunk = _

  def this(chunk: Chunk) = {
    this()
    this.chunk = chunk
  }

  override def writeExternal(out: ObjectOutput): Unit = {
    val kryo = GlobalKryo()
    val output = new io.Output(ObjectOutputAsStream(out), 8000)
    kryo.writeObject(output, chunk)
  }

  override def readExternal(in: ObjectInput): Unit = {
    val kryo = GlobalKryo()
    val input = new io.Input(ObjectInputAsStream(in), 8000)
    input.setLimit(8000)
    chunk = kryo.readObject(input, classOf[Chunk])
  }
}

class HybridSerialService extends SaveSerialService {

  override def write(path: Path, obj: Map[V3I, Chunk]): Unit = {
    val out = new ObjectOutputStream(new FileOutputStream(path.toFile))
    val toWrite = obj
      .mapValues(new HybridChunkWrapper(_))
      .foldLeft(new HashMap[V3I, HybridChunkWrapper])({ case (map, (key, value)) => map.updated(key, value) })
    out.writeObject(toWrite)
    out.close()
  }

  override def read(path: Path): Map[V3I, Chunk] = {
    if (path.toFile.exists) {
      val in = new ObjectInputStream(new FileInputStream(path.toFile))
      val read = in.readObject().asInstanceOf[Map[V3I, HybridChunkWrapper]]
      in.close()
      read.mapValues(_.chunk)
    } else Map.empty
  }
}

class CarboniteSerialService extends SaveSerialService {
  implicit val config = EgressCarboniteConfig

  override def write(path: Path, obj: Map[V3I, Chunk]): Unit = {
    val out = new CarboniteOutputStream(new FileOutputStream(path.toFile))
    out.writeObject(obj)
    out.close()
  }

  override def read(path: Path): Map[V3I, Chunk] = {
    if (path.toFile.exists) {
      val in = new CarboniteInputStream(new FileInputStream(path.toFile))
      try in.readObject().asInstanceOf[Map[V3I, Chunk]]
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
        var map = serial.read(path)
        //var map: Map[V3I, Chunk] = serial.read(path).map(_.asInstanceOf[Map[V3I, Chunk]]).getOrElse(Map.empty)
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
        //val pulled: Map[V3I, Chunk] =
        //  serial.read(pathFor(region)).map(_.asInstanceOf[Map[V3I, Chunk]]).getOrElse(Map.empty)
        val pulled = serial.read(pathFor(region))
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
