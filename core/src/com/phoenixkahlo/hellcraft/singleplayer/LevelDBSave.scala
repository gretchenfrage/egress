package com.phoenixkahlo.hellcraft.singleplayer

import java.nio.file.Path

import com.phoenixkahlo.hellcraft.core.{Chunk, Terrain}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.threading._
import org.iq80.leveldb._
import org.fusesource.leveldbjni.JniDBFactory._
import java.io._

import com.phoenixkahlo.hellcraft.util.collections.ParGenMutHashMap

import scala.collection.mutable.ArrayBuffer

class LevelDBSave(path: Path, generator: Generator) extends AsyncSave {
  @volatile private var closing = false

  private val db: DB = {
    val options = new Options
    options.createIfMissing(true)
    factory.open(path.toFile, options)
  }
  private val sequences = new ParGenMutHashMap[V3I, FutSequences](p => new FutSequences(UniExecutor.db(p * 16)))

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
      futs += sequences(p)()({
        db.put(p.toByteArray, serialize(chunk))
      })
    }
    futs
  }

  override def close(chunks: Map[V3I, Chunk]): Seq[Fut[Unit]] = {
    // disrupt pull futures
    closing = true
    // disrupt generator
    generator.cancel()
    // drain the db 3d queue into the db seq queue to reduce tree overhead
    UniExecutor.getService.makeDBSequential()
    // add the pushes to all the sequences
    for ((p, chunk) <- chunks) {
      sequences(p)()({
        db.put(p.toByteArray, serialize(chunk))
      })
    }
    // fold all sequences into a promise that all operations are completed
    val finish: Fut[Unit] = PromiseFold(sequences.toSeq.map(_._2.getLast))
    // after that, close the database
    val close: Fut[Unit] = finish.afterwards(db.close(), UniExecutor.db)
    // return that
    Seq(close)
  }



  /*
  override def pull(chunks: Seq[V3I]): Map[V3I, Fut[Chunk]] = {
    var map = Map.empty[V3I, Fut[Chunk]]
    for (p <- chunks) {
      map = map.updated(p, sequences(p)()({
        if (!closing) {
          val bytes = db.get(p.toByteArray)
          if (bytes != null) Fut(deserialize(bytes), _.run())
          else generator.chunkAt(p)
        } else Fut(null: Chunk, _.run())
      }).flatMap(identity))
    }
    map
  }
  */
  override def pull(chunks: Seq[V3I], terrain: Seq[V3I]): (Map[V3I, Fut[Chunk]], Map[V3I, Fut[Terrain]]) = {
    var map = Map.empty[V3I, Fut[Chunk]]
    for (p <- chunks) {
      map = map.updated(p, sequences(p)()({
        if (!closing) {
          val bytes = db.get(p.toByteArray)
          if (bytes != null) Fut(deserialize(bytes), _.run())
          else generator.chunkAt(p)
        } else Fut(null: Chunk, _.run())
      }).flatMap(identity))
    }
    (map, terrain.map(p => p -> generator.terrainAt(p)).toMap)
  }
}
