package com.phoenixkahlo.hellcraft.util.collections

import java.util.concurrent.ConcurrentLinkedQueue

import com.phoenixkahlo.hellcraft.util.threading.Fut

import scala.collection.parallel

/**
  * Memoizing function.
  */
class MemoFunc[I, O](func: I => O) extends (I => O) {
  private val map = new ParGenMutHashMap[I, Fut[O]](_ => ???)

  override def apply(input: I): O = {
    //var creator: Runnable = null
    val exec = new ConcurrentLinkedQueue[Runnable]
    val fut = map(input)(Fut(func(input), exec.add))
    //if (creator != null)
    //  creator.run()
    while (exec.size > 0)
      exec.remove().run()
    fut.await
  }
}

class MemoHintFunc[I, H, O](func: (I, H) => O) extends ((I, H) => O) {
  private val map = new ParGenMutHashMap[I, Fut[O]](_ => ???)

  override def apply(input: I, hint: H): O = {
    var creator: Runnable = null
    val fut = map(input)(Fut(func(input, hint), creator = _))
    if (creator != null)
      creator.run()
    fut.await
  }
}