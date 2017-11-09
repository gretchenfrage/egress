package com.phoenixkahlo.hellcraft.util.collections

import com.phoenixkahlo.hellcraft.util.threading.Fut

import scala.collection.parallel

/**
  * Memoizing function.
  */
class MemoFunc[I, O](func: I => O) extends (I => O) {
  private val creators = new parallel.mutable.ParHashMap[I, Runnable]
  private val map = new ParGenMutHashMap[I, Fut[O]](i => Fut(func(i), creators.put(i, _)))

  override def apply(input: I): O = {
    val fut = map(input)
    creators.get(input).foreach(_.run())
    creators.remove(input)
    fut.query.get
  }
}
