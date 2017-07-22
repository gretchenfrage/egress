package com.phoenixkahlo.hellcraft.infinitetest

import java.util.concurrent.LinkedBlockingDeque

import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.math.{Directions, V3I}
import com.phoenixkahlo.hellcraft.save.WorldSave
import com.phoenixkahlo.hellcraft.util.PriorityExecContext

import scala.collection.immutable.HashMap
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Wraps a save and references a world, and keeps a buffer of chunks near to where the player is,
  * loading them and unloading them in the background.
  */
class SaveBuffer(val save: WorldSave, generator: V3I => Block) {

  // the chunk buffer
  private var buffer: Map[V3I, Either[Chunk, Future[Chunk]]] = new HashMap
  // futures for the chunks being saved to the hard drive
  private var savings: Map[V3I, Future[Unit]] = new HashMap

  // the load/unload thread and different contexts to use it
  private val queue = new LinkedBlockingDeque[Runnable]
  val thread = new Thread(() => {
    while (true) {
      queue.take().run()
    }
  })
  thread.setName("Save file thread")
  thread.setPriority(3)
  thread.start()
  private val enqueueLast = new ExecutionContext {
    override def reportFailure(cause: Throwable): Unit = println("enqueueLast failure reported: " + cause)
    override def execute(runnable: Runnable): Unit = queue.addLast(runnable)
  }
  private val enqueueFirst = new ExecutionContext {
    override def reportFailure(cause: Throwable): Unit = println("enqueueFirst failure reported: " + cause)
    override def execute(runnable: Runnable): Unit = queue.addFirst(runnable)
  }

  def push(chunks: Seq[Chunk]): Unit = this.synchronized {
    buffer = chunks.foldLeft(buffer)({ case (buff, chunk) => buff.updated(chunk.pos, Left(chunk)) })
  }

  def pull(ps: Seq[V3I]): Map[V3I, Chunk] = this.synchronized {
    // wait for any possible save operations
    for (v <- ps)
      savings.get(v).foreach(Await.result(_, Duration.Inf))

    // fill the buffer with futures for loading any chunks that must be loaded
    val toLoad = ps.filterNot(buffer.contains)
    val loadFuture: Future[Map[V3I, Chunk]] = Future { save.load(toLoad) } (enqueueFirst)
    val futureMap: Map[V3I, Future[Chunk]] = toLoad.map(v => (v, loadFuture.transform(_.get(v) match {
      case Some(chunk) => chunk
      case None => new Chunk(v).mapBlocks(lv => generator(v * 16 + lv))
    }, identity)(PriorityExecContext(2)))).toMap
    buffer = futureMap.foldLeft(buffer)({ case (buff, (v, future)) => buff.updated(v, Right(future)) })

    // collect the chunks from the buffer, awaiting futures as necessary
    val chunks: Map[V3I, Chunk] = ps.map(v => buffer(v) match {
      case Left(chunk) => (v, chunk)
      case Right(future) => (v, Await.result(future, Duration.Inf))
    }).toMap

    // return the chunks
    chunks
  }

  def update(loaded: Seq[V3I], thickness: Int, world: World): Unit = this.synchronized {
    // clean up save futures that are completed
    savings = savings.filterNot({ case (_, future) => future.isCompleted })

    // calculate which chunks should be buffered
    def inflate(set: Set[V3I], thickness: Int): Set[V3I] = {
      if (thickness <= 0) set
      else if (thickness == 1) set ++ set.flatMap(v => Directions().map(_ + v))
      else inflate(inflate(set, 1), thickness - 1)
    }
    val target: Set[V3I] = inflate(loaded.toSet, thickness).filterNot(savings.contains)

    // add chunk-loading futures to the buffer
    val toLoad = target.filterNot(buffer.contains).toSeq
    val loadFuture: Future[Map[V3I, Chunk]] = Future { save.load(toLoad) } (enqueueLast)
    val loadFutureMap: Map[V3I, Future[Chunk]] = toLoad.map(v => (v, loadFuture.transform(_.get(v) match {
      case Some(chunk) => chunk
      case None => new Chunk(v).mapBlocks(lv => generator(v * 16 + lv))
    }, identity)(PriorityExecContext(1)))).toMap
    buffer = loadFutureMap.foldLeft(buffer)({ case (buff, (v, future)) => buff.updated(v, Right(future)) })

    // add chunk-saving futures and remove those chunks from the buffer
    val toSave = buffer.keys.filterNot(target.contains).toSeq
    val chunksToSave = toSave.map(v => buffer(v) match {
      case Left(chunk) => chunk
      case Right(future) => Await.result(future, Duration.Inf)
    })
    // first, we must wait for previous savings of that chunk to occur. this will matter in the case of override-pushes
    for (v <- toSave)
      savings.get(v).foreach(Await.result(_, Duration.Inf))
    // then we can start our new saving operation
    val saveFuture: Future[Unit] = Future { save.save(chunksToSave, world) } (enqueueLast)
    savings = toSave.foldLeft(savings)({ case (svngs, v) => svngs.updated(v, saveFuture) })
    // and finally, remove the saved chunks from the buffer
    buffer --= toSave
  }

  def resources(textures: TexturePack, world: World): Seq[ResourceNode] = this.synchronized {
    //buffer.values.filter(_ isLeft).map({ case Left(chunk) => chunk })
    buffer.values.flatMap({
      case Left(chunk) => Some(chunk)
      case Right(_) => None
    }).flatMap(_.renderables(textures, world)).flatMap(_.resources).toSeq
  }

  def close(world: World): Unit = this.synchronized {
    // wait for existing save futures
    savings.values.foreach(future => Await.result(future, Duration.Inf))
    // save all remaining chunks
    val chunks = buffer.keys.map(v => buffer(v) match {
      case Left(chunk) => chunk
      case Right(future) => Await.result(future, Duration.Inf)
    }).toSeq
    val future: Future[Unit] = Future { save.save(chunks, world) } (enqueueLast)
    Await.result(future, Duration.Inf)
    // destroy this object
    buffer = null
    savings = null
  }

}