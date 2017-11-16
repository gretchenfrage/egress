package com.phoenixkahlo.hellcraft.util.collections

import com.phoenixkahlo.hellcraft.util.threading.Fut

import scala.collection.parallel

/**
  * Memoizing function.
  */
class MemoFunc[I, O](func: I => O) extends (I => O) {
  private val map = new ParGenMutHashMap[(I, Runnable => Unit), Fut[O]]({ case (i, e) => Fut(func(i), e) })

  override def apply(input: I): O = {
    var creator: Runnable = null
    val fut = map((input, task => creator = task))
    if (creator != null)
      creator.run()
    fut.query.get
  }
}
