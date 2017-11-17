package com.phoenixkahlo.hellcraft.singleplayer

import java.nio.file.Path

import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.threading.{Fut, FutSequences, UniExecutor}
import org.iq80.leveldb._
import org.fusesource.leveldbjni.JniDBFactory._
import java.io._

import com.phoenixkahlo.hellcraft.util.collections.ParGenMutHashMap

import scala.collection.mutable.ArrayBuffer

class LevelDBSave(path: Path, generator: V3I => Fut[Chunk]) extends AsyncSave {
  private val db: DB = {
    val options = new Options
    options.createIfMissing(true)
    factory.open(path.toFile, options)
  }
  private val sequences = new ParGenMutHashMap[V3I, FutSequences](p => new FutSequences(UniExecutor.exec(p * 16)))

  private def serialize(chunk: Chunk): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(chunk)
    oos.flush()
    baos.toByteArray
  }

  private def deserialize(bytes: Array[Byte]): Chunk = {
    val bais = new ByteArrayInputStream(bytes)
    val ois = new ObjectInputStream(bais)
    ois.readObject().asInstanceOf[Chunk]
  }

  override def push(chunks: Map[V3I, Chunk]): Seq[Fut[Unit]] = {
    val futs = new ArrayBuffer[Fut[Unit]]
    for ((p, chunk) <- chunks) {
      futs += sequences(p)({
        db.put(p.toByteArray, serialize(chunk))
      })
    }
    futs
  }

  override def finalPush(chunks: Map[V3I, Chunk]): Seq[Fut[Unit]] = {
    val futs = new ArrayBuffer[Fut[Unit]]
    for ((p, chunk) <- chunks) {
      futs += sequences(p)({
        db.put(p.toByteArray, serialize(chunk))
      })
    }
    // TODO: close the database
    futs
  }

  override def pull(chunks: Seq[V3I]): Map[V3I, Fut[Chunk]] = {
    var map = Map.empty[V3I, Fut[Chunk]]
    for (p <- chunks) {
      map = map.updated(p, sequences(p)({
        val bytes = db.get(p.toByteArray)
        if (bytes != null) Fut(deserialize(bytes), _.run())
        else generator(p)
        //deserialize(db.get(p.toByteArray))
      }).flatMap(identity))
    }
    map
  }
}
