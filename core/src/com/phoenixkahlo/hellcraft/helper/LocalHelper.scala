package com.phoenixkahlo.hellcraft.helper
import com.phoenixkahlo.hellcraft.core.eval.ExecHint
import com.phoenixkahlo.hellcraft.util.collections.{IdentityKey, TypeMatchingMap}
import com.phoenixkahlo.hellcraft.helper.HM._
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

import scala.collection.parallel.mutable.ParHashMap
import scala.collection.{mutable, parallel}

class LocalHelper(implicit executor: UniExecutor) extends Helper {
  //var remotes: TypeMatchingMap[HelperServiceID, RemoteHelperServiceProcedure, HelperService] = null
  //var locals: TypeMatchingMap[HelperServiceID, LocalHelperServiceProcedure, HelperService] = null
  //var remoteAccessors: TypeMatchingMap[HelperServiceID, RemoteHelperServiceProcedureAccessor, HelperService] = null
  //var localAccessors: TypeMatchingMap[HelperServiceID, LocalHelperServiceProcedureAccessor, HelperService] = null
  var remotes: Map[HelperServiceID[_], RemoteHelperServiceProcedure[_]] = null
  var locals: Map[HelperServiceID[_], LocalHelperServiceProcedure[_]] = null
  var remoteAccessors: Map[HelperServiceID[_], RemoteHelperServiceProcedureAccessor[_]] = null
  var localAccessors: Map[HelperServiceID[_], LocalHelperServiceProcedureAccessor[_]] = null


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
            remotes(monad.sid).asInstanceOf[RemoteHelperServiceProcedure[S]].invoke(monad.call, localAccessors(monad.sid).asInstanceOf[LocalHelperServiceProcedureAccessor[S]])
          f(monad)
      }
      accum.put(IdentityKey(monad), fut)
      fut
    })

  override def apply[T](monad: HM[T]): Fut[T] = compile(monad, new ParHashMap)

  override def start(services: Seq[HelperService.Starter[_ <: HelperService]]): Unit = this.synchronized {
    //remotes = TypeMatchingMap.empty[HelperServiceID, RemoteHelperServiceProcedure, HelperService]
    //locals = TypeMatchingMap.empty[HelperServiceID, LocalHelperServiceProcedure, HelperService]
    remotes = Map.empty
    locals = Map.empty
    remoteAccessors = Map.empty
    localAccessors = Map.empty

    def start[S <: HelperService](starter: HelperService.Starter[S]): Unit = {
      locals += ((starter.id, starter.local))
      remotes += ((starter.id, starter.remote(executor)))
      localAccessors += ((starter.id, new LocalHelperServiceProcedureAccessor[S] {
        override def invoke[T](call: S#LocalCall[T]): Fut[T] =
          locals(starter.id).asInstanceOf[LocalHelperServiceProcedure[S]].invoke(call, remoteAccessors(starter.id).asInstanceOf[RemoteHelperServiceProcedureAccessor[S]])
      }))
      remoteAccessors += ((starter.id, new RemoteHelperServiceProcedureAccessor[S] {
        override def invoke[T](call: S#RemoteCall[T]): Fut[T] =
          remotes(starter.id).asInstanceOf[RemoteHelperServiceProcedure[S]].invoke(call, localAccessors(starter.id).asInstanceOf[LocalHelperServiceProcedureAccessor[S]])
      }))
    }
    services.foreach(start(_))
  }

  override def close(): Unit = this.synchronized {
    for ((id: HelperServiceID[HelperService], remote: RemoteHelperServiceProcedure[HelperService]) <- remotes.toSeq)
      remote.close()
    remotes = null
    for ((id: HelperServiceID[HelperService], local: LocalHelperServiceProcedure[HelperService]) <- locals.toSeq)
      local.close()
    locals = null
  }
}
