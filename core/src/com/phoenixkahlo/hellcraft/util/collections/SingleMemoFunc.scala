package com.phoenixkahlo.hellcraft.util.collections

import java.util
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.phoenixkahlo.hellcraft.util.threading.Fut

class SingleMemoFunc[I, O](func: I => O) extends (I => O) {
  private var curr: Option[(I, Fut[O])] = None
  private val lock = new ReentrantReadWriteLock

  override def apply(input: I): O = {
    lock.readLock().lock()
    curr match {
      case Some((i, fut)) if i == input =>
        lock.readLock.unlock()
        fut.await
      case _ =>
        lock.readLock().unlock()
        lock.writeLock().lock()
        curr match {
          case Some((i, fut)) if i == input =>
            lock.writeLock.unlock()
            fut.await
          case _ =>
            val exec = new util.LinkedList[Runnable]
            val fut = Fut(func(input), exec.add(_))
            curr = Some(input -> fut)
            lock.writeLock().unlock()
            while (exec.size > 0)
              exec.remove().run()
            fut.await
        }
    }
  }
}


class SingleMemoHintFunc[I, H, O](func: (I, H) => O) extends ((I, H) => O) {
  private var curr: Option[(I, Fut[O])] = None
  private val lock = new ReentrantReadWriteLock

  override def apply(input: I, hint: H): O = {
    lock.readLock().lock()
    curr match {
      case Some((i, fut)) if i == input =>
        lock.readLock.unlock()
        fut.await
      case _ =>
        lock.readLock().unlock()
        lock.writeLock().lock()
        curr match {
          case Some((i, fut)) if i == input =>
            lock.writeLock.unlock()
            fut.await
          case _ =>
            val exec = new util.LinkedList[Runnable]
            val fut = Fut(func(input, hint), exec.add(_))
            curr = Some(input -> fut)
            lock.writeLock().unlock()
            while (exec.size > 0)
              exec.remove().run()
            fut.await
        }
    }
  }
}