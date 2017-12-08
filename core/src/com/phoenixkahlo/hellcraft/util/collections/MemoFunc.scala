package com.phoenixkahlo.hellcraft.util.collections

import java.util.concurrent.ConcurrentLinkedQueue

import com.phoenixkahlo.hellcraft.util.collections.GenFunc.GenWrapper
import com.phoenixkahlo.hellcraft.util.threading.Fut

import scala.collection.parallel

/**
  * Memoizing function.
  */
class MemoFunc[I, O](func: I => O) extends (I => O) {
  private val map = new ParGenMutHashMap[I, Fut[O]](_ => ???)

  override def apply(input: I): O = {
    val exec = new ConcurrentLinkedQueue[Runnable]
    val fut = map(input)(Fut(func(input), exec.add(_)))
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

trait GenFunc[I[_], O[_]] {
  def apply[E](i: I[E]): O[E]
}
object GenFunc {
  type GenWrapper[O[_]] = GenFunc[Identity, O]
  type GenUnwrapper[I[_]] = GenFunc[I, Identity]
}

abstract class GenMemoFunc[I[_], O[_]] extends GenFunc[I, O] {
  private val map = new ParGenMutHashMap[I[_], Fut[O[_]]](_ => ???)

  protected def gen[E](i: I[E]): O[E]

  override def apply[E](i: I[E]): O[E] = {
    val exec = new ConcurrentLinkedQueue[Runnable]
    val fut = map(i)(Fut(gen(i), exec.add(_)))
    while (exec.size > 0)
      exec.remove().run()
    fut.await.asInstanceOf[O[E]]
  }
}

object GenMemoFuncTest extends App {
  // we demonstrate a generic memoizing boxing function that wraps an element into a tuple
  val box: GenWrapper[Tuple1] = new GenMemoFunc[Identity, Tuple1] {
    override protected def gen[E](i: E): Tuple1[E] = {
      println("boxing " + i)
      Tuple1(i)
    }
  }

  println(box(5))
  println(box(5))
  println(box(5))
  println(box(6))
  println(box("hello world"))

}