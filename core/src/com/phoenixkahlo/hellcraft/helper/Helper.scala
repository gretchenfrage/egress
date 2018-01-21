package com.phoenixkahlo.hellcraft.helper

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.eval.ExecHint
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

/**
  * The helper monad.
  * Another type of evaluation monad.
  * To be sent over to a helper to represent a remote fut.
  * Contains the usual suspects, as well as one derived from a (UniExecutor => Fut[T]),
  * and one to invoke a helper service.
  */
sealed trait HM[+T] extends Serializable {
  def map[R](func: T => R)(implicit exec: ExecHint): HM[R] = HM.HMMap(this, func, exec)
  def flatMap[R](func: T => HM[R]): HM[R] = HM.HMFMap(this, func)
  def filter(test: T => Boolean): HM[T] = this
  def withFilter(test: T => Boolean): HM[T] = this
}
object HM {
  def apply[T](fac: => T)(implicit exec: ExecHint): HM[T] = HMGen(() => fac, exec)
  def fromFut[T](fac: UniExecutor => Fut[T]): HM[T] = HMMisc(fac)

  case class HMGen[T](fac: () => T, exec: ExecHint) extends HM[T]
  case class HMMap[S, R](src: HM[S], func: S => R, exec: ExecHint) extends HM[R]
  case class HMFMap[S, R](src: HM[S], func: S => HM[R]) extends HM[R]
  case class HMMisc[T](fac: UniExecutor => Fut[T]) extends HM[T]
  case class HMSCall[T, S <: HelperService](sid: HelperServiceID[S], call: S#RemoteCall[T]) extends HM[T]
}

/**
  * Helper services are stateful services on the remote end of a helper that evaluate futs.
  * For example, it could represent a database, or a network socket.
  */
trait HelperService {
  type RemoteCall[T]
  type LocalCall[T]
}
object HelperService {
  /**
    * Bean used to start service.
    */
  case class Starter[S <: HelperService](
                                          id: HelperServiceID[S],
                                          local: LocalHelperServiceProcedure[S],
                                          remote: UniExecutor => RemoteHelperServiceProcedure[S]
                                               )
}

/**
  * A simple typesafe wrapper around a UUID with a convience method for making a service call helper monad.
  */
case class HelperServiceID[S <: HelperService](id: UUID) {
  def call[T](call: S#RemoteCall[T]): HM[T] = HM.HMSCall[T, S](this, call)
}

/**
  * The stateful service object that lives on the remote end.
  */
trait RemoteHelperServiceProcedure[S <: HelperService] {
  def invoke[T](call: S#RemoteCall[T], other: LocalHelperServiceProcedureAccessor[S]): Fut[T]

  def close(): Unit
}

/**
  * The stateful service object on the local end.
  */
trait LocalHelperServiceProcedure[S <: HelperService] {
  def invoke[T](call: S#LocalCall[T], other: RemoteHelperServiceProcedureAccessor[S]): Fut[T]

  def close(): Unit
}

/**
  * The interface the local procedure uses to access the remote procedure.
  */
trait RemoteHelperServiceProcedureAccessor[S <: HelperService] {
  def invoke[T](call: S#RemoteCall[T]): Fut[T]
}

/**
  * The interface the remote procedure uses to access the local procedure.
  */
trait LocalHelperServiceProcedureAccessor[S <: HelperService] {
  def invoke[T](call: S#LocalCall[T]): Fut[T]
}

/**
  * The actual helper trait (lives on the remote end).
  */
trait Helper {
  def apply[T](monad: HM[T]): Fut[T]

  def start(services: Seq[HelperService.Starter[_ <: HelperService]]): Unit
  def close(): Unit
}
