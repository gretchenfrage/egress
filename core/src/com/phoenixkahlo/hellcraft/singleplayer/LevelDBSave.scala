package com.phoenixkahlo.hellcraft.singleplayer

import java.nio.file.Path

import com.phoenixkahlo.hellcraft.core.{Chunk, Terrain}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.threading._
import org.iq80.leveldb._
import org.fusesource.leveldbjni.JniDBFactory._
import java.io._
import java.nio.ByteBuffer

import com.phoenixkahlo.hellcraft.singleplayer.AsyncSave.{DataKey, GetPos}
import com.phoenixkahlo.hellcraft.util.collections.ParGenMutHashMap
import com.phoenixkahlo.hellcraft.util.debugging.Profiler

import scala.collection.mutable.ArrayBuffer

class LevelDBSave[T <: GetPos with Serializable](path: Path, generator: Generator, elevate: Chunk => T) extends AsyncSave[T] {
  @volatile private var closing = false

  private val db: DB = {
    val options = new Options
    options.createIfMissing(true)
    factory.open(path.toFile, options)
  }
  private val chunkSeqs = new ParGenMutHashMap[V3I, FutSequence](p => new FutSequence(UniExecutor.execc(p * 16)))
  private val keySeqs = new ParGenMutHashMap[DataKey[_], FutSequence](key => new FutSequence(UniExecutor.execc))
  private val sequencer = new FutSequence(UniExecutor.execc)

  private def serialize(chunk: Any): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(chunk)
    oos.flush()
    baos.toByteArray
  }

  private def deserialize[E](bytes: Array[Byte]): E = {
    val bais = new ByteArrayInputStream(bytes)
    val ois = new ObjectInputStream(bais)
    ois.readObject().asInstanceOf[E]
  }

  private def encode(key: Long): Array[Byte] = {
    val buffer = ByteBuffer.allocate(java.lang.Long.BYTES)
    buffer.putLong(key)
    buffer.array()
  }

  override def push(chunks: Map[V3I, T]): Fut[Unit] = {
    sequencer[Fut[Unit]](() => {
      val promises: Seq[Fut[Unit]] =
        chunks.toSeq.map({ case (p, chunk) => chunkSeqs(p)()[Unit](() => db.put(p.toByteArray, serialize(chunk))) })
      PromiseFold(promises)
    }).flatten
  }

  override def pull(chunks: Seq[V3I], terrain: Seq[V3I]): (Map[V3I, Fut[T]], Map[V3I, Fut[Terrain]]) = {
    val p = Profiler("save.pull: " + chunks.size + " chunks")
    var map = Map.empty[V3I, Fut[T]]
    for (p <- chunks) {
      map += (p -> sequencer(() => chunkSeqs(p)()(() => {
        if (!closing) {
          val bytes = db.get(p.toByteArray)
          if (bytes != null) Fut(deserialize[T](bytes), _.run())
          else generator.chunkAt(p).map(elevate)
        } else Fut(null.asInstanceOf[T], _.run())
      })).flatten.flatten)
    }
    p.log()
    val tmap = terrain.map(p => p -> Fut(generator.terrainAt(p), UniExecutor.exec).flatten).toMap
    p.log()
    p.printDisc(20)

    (map, tmap)
  }

  override def putKey[K](key: AsyncSave.DataKey[K], value: K): Promise = {
    sequencer[Fut[Unit]](() => keySeqs(key)()[Unit](() => db.put(encode(key.code), serialize(value)))).flatten
  }

  override def getKey[K](key: AsyncSave.DataKey[K]): Fut[K] = {
    sequencer(() => keySeqs(key)()(() => {
      if (!closing) {
        val bytes = db.get(encode(key.code))
        if (bytes != null) deserialize[K](bytes)
        else key.default
      } else null.asInstanceOf[K]
    })).flatten
  }

  override def close(chunks: Map[V3I, T]): Fut[Unit] = {
    // disrupt pull futures
    closing = true
    // disrupt generator
    generator.cancel()
    // drain the db 3d queue into the db seq queue to reduce tree overhead
    UniExecutor.getService.makeCSequential()
    // add the pushes to all the sequences
    for ((p, chunk) <- chunks) {
      sequencer(() => chunkSeqs(p)()(() => db.put(p.toByteArray, serialize(chunk))))
    }
    // fold all sequences into a promise that all operations are completed, and then close it
    sequencer(() => PromiseFold(chunkSeqs.toSeq.map(_._2.getLast) ++ keySeqs.toSeq.map(_._2.getLast))
      .afterwards(() => db.close(), UniExecutor.execc)).flatten
  }
}
