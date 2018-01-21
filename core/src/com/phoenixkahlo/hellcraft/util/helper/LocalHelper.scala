package com.phoenixkahlo.hellcraft.util.helper
import com.phoenixkahlo.hellcraft.core.eval.ExecHint
import com.phoenixkahlo.hellcraft.util.collections.{IdentityKey, TypeMatchingMap}
import com.phoenixkahlo.hellcraft.util.helper.HM._
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

import scala.collection.parallel.mutable.ParHashMap
import scala.collection.{mutable, parallel}

class LocalHelper(implicit executor: UniExecutor) extends Helper {
  var remotes: TypeMatchingMap[ServiceID, RemoteHelperServiceProcedure, HelperService] = null
  var locals: TypeMatchingMap[ServiceID, LocalHelperServiceProcedure, HelperService] = null
  var remoteAccessors: TypeMatchingMap[ServiceID, RemoteHelperServiceProcedureAccessor, HelperService] = null
  var localAccessors: TypeMatchingMap[ServiceID, LocalHelperServiceProcedureAccessor, HelperService] = null

  private def compile[T](monad: HM[T], accum: parallel.mutable.ParMap[IdentityKey[HM[_]], Fut[_]]): Fut[T] =
    accum.get(IdentityKey(monad)).asInstanceOf[Option[Fut[T]]].getOrElse({
      val fut = monad match {
        case HMGen(fac: (() => T), exec: ExecHint) => Fut(fac(), exec.exec)
        case monad: HMMap[_, T] =>
          def f[S](monad: HMMap[S, T]): Fut[T] =
            compile(monad.src, accum).map(monad.func, monad.exec.exec)
          f(monad)
        case monad: HMFMap[_, T] =>
          def f[S](monad: HMFMap[S, T]): Fut[T] =
            compile(monad.src, accum).flatMap(s => compile(monad.func(s), accum))
          f(monad)
        case HMMisc(fac: (UniExecutor => Fut[T])) => fac(executor)
        case monad: HMSCall[T, _] =>
          def f[S <: HelperService](monad: HMSCall[T, S]): Fut[T] =
            remotes(monad.sid).invoke(monad.call, localAccessors(monad.sid))
          f(monad)
      }
      accum.put(IdentityKey(monad), fut)
      fut
    })

  override def apply[T](monad: HM[T]): Fut[T] = compile(monad, new ParHashMap)

  override def start(services: Seq[HelperService.Starter[_ <: HelperService]]): Unit = this.synchronized {
    remotes = TypeMatchingMap.empty[ServiceID, RemoteHelperServiceProcedure, HelperService]
    locals = TypeMatchingMap.empty[ServiceID, LocalHelperServiceProcedure, HelperService]

    def start[S <: HelperService](starter: HelperService.Starter[S]): Unit = {
      locals += ((starter.id, starter.local))
      remotes += ((starter.id, starter.remote(executor)))
      localAccessors += ((starter.id, new LocalHelperServiceProcedureAccessor[S] {
        override def invoke[T](call: S#LocalCall[T]) =
          locals(starter.id).invoke(call, remoteAccessors(starter.id))
      }))
      remoteAccessors += (starter.id, new RemoteHelperServiceProcedureAccessor[S] {
        override def invoke[T](call: S#RemoteCall[T]): Fut[T] =
          remotes(starter.id).invoke(call, localAccessors(starter.id))
      })
    }
  }

  override def close(): Unit = this.synchronized {
    for ((id: ServiceID[_], remote: RemoteHelperServiceProcedure[_]) <- remotes.toSeq)
      remote.close()
    remotes = null
    for ((id: ServiceID[_], local: LocalHelperServiceProcedure[_]) <- locals.toSeq)
      local.close()
    locals = null
  }
}
