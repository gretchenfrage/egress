package com.phoenixkahlo.hellcraft.util.collections

import java.util
import java.util.UUID
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.phoenixkahlo.hellcraft.util.threading.Fut

trait GenHintFunc[I[_ <: B], H[_ <: B], O[_ <: B], B] {
  def apply[E <: B](i: I[E], h: H[E]): O[E]
}

abstract class TransSingleMemoGenHintFunc[I[_ <: B], H[_ <: B], O[_ <: B], B] extends GenHintFunc[I, H, O, B] with Serializable {
  protected def gen[E <: B](i: I[E], h: H[E]): O[E]

  @transient private var curr: Option[(I[_], Fut[O[_]])] = None
  @transient private lazy val lock = new ReentrantReadWriteLock

  override def apply[E <: B](input: I[E], hint: H[E]): O[E] = {
    lock.readLock().lock()
    curr match {
      case Some((i, fut)) if i == input =>
        lock.readLock.unlock
        fut.await.asInstanceOf[O[E]]
      case _ =>
        lock.readLock().unlock()
        lock.writeLock().lock()
        curr match {
          case Some((i, fut)) if i == input =>
            lock.readLock.unlock
            fut.await.asInstanceOf[O[E]]
          case _ =>
            val exec = new util.LinkedList[Runnable]
            val fut = Fut(gen(input, hint), exec.add(_))
            curr = Some(input -> fut)
            lock.writeLock().unlock()
            while (exec.size > 0)
              exec.remove().run()
            fut.await
        }
    }
  }
}

object ContextPin {
  type ContextPinID[T] = UUID
  type ContextPinFunc[T] = () => T
  type ContextPin = TransSingleMemoGenHintFunc[ContextPinID, ContextPinFunc, Identity, Any]
  def create(): ContextPin = new ContextPin {
    override protected def gen[E <: Any](id: UUID, factory: () => E): E = {
      factory()
    }
  }
}

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

  def query(input: I): Option[O] = {
    lock.readLock().lock()
    val o = curr.filter(_._1 == input).flatMap(_._2.query)
    lock.readLock().unlock()
    o
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

  def query(input: I): Option[O] = {
    lock.readLock().lock()
    val o = curr.filter(_._1 == input).flatMap(_._2.query)
    lock.readLock().unlock()
    o
  }
}