package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.{Chunk, Terrain}
import com.phoenixkahlo.hellcraft.core.entity.{AnyEnt, AnyEntID, EntID, Entity}
import com.phoenixkahlo.hellcraft.core.eval.{Eval, ExecHint, WEval}
import com.phoenixkahlo.hellcraft.core.eval.Eval.{ECreate, EFMap, EFilter, EMap}
import com.phoenixkahlo.hellcraft.core.eval.WEval.{WEval, WEvalChunk, WEvalEnt, WEvalTerrain}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.singleplayer.WEvalHelperService.{GetChunk, GetEnt, GetTerrain}
import com.phoenixkahlo.hellcraft.util.collections.IdentityKey
import com.phoenixkahlo.hellcraft.helper._
import com.phoenixkahlo.hellcraft.util.threading.{FulfillmentContext, Fut, UniExecutor}

import scala.collection.{mutable, parallel}

trait WEvalHelperService extends HelperService {
  override type RemoteCall[T] = WEvalHelperService.RemoteCall[T]
  override type LocalCall[T] = WEvalHelperService.LocalCall[T]
}
object WEvalHelperService {
  sealed trait RemoteCall[T]
  sealed trait LocalCall[T]

  case class GetChunk(p: V3I) extends RemoteCall[GetChunk] with LocalCall[GetChunk]
  case class GetTerrain(p: V3I) extends RemoteCall[GetTerrain] with LocalCall[GetTerrain]
  case class GetEnt[E <: Entity[_]](id: EntID[E]) extends RemoteCall[E] with LocalCall[E]

  case class Eval[T](eval: WEval[T]) extends RemoteCall[T]
}

object WEvalHelperServiceID extends HelperServiceID[WEvalHelperService](UUID.randomUUID())

class WEvalHelperServiceLocalProcedure(
                                        cfulfill: FulfillmentContext[V3I, Chunk],
                                        tfulfill: FulfillmentContext[V3I, Terrain],
                                        efulfill: FulfillmentContext[AnyEntID, AnyEnt]
                                      ) extends LocalHelperServiceProcedure[WEvalHelperService] {
  override def invoke[T](call: WEvalHelperService.LocalCall[T],
                         other: RemoteHelperServiceProcedureAccessor[WEvalHelperService]): Fut[T] = call match {
    case GetChunk(p) => cfulfill.fut(p).asInstanceOf[Fut[T]]
    case GetTerrain(p) => tfulfill.fut(p).asInstanceOf[Fut[T]]
    case GetEnt(id) => efulfill.fut(id.asInstanceOf[AnyEntID]).asInstanceOf[Fut[T]]
  }

  override def close(): Unit = ()
}

class WEvalHelperServiceRemoteProcedure(implicit executor: UniExecutor) extends RemoteHelperServiceProcedure[WEvalHelperService] {
  private def compile[T](monad: WEval[T])(implicit accum: parallel.mutable.ParMap[IdentityKey[WEval[_]], Fut[_]],
                                          other: LocalHelperServiceProcedureAccessor[WEvalHelperService]): Fut[T] =
    accum.get(IdentityKey(monad)).asInstanceOf[Option[Fut[T]]].getOrElse({
      val fut = monad match {
        case ECreate(fac: (() => T), hint) => Fut(fac(), hint.exec)
        case monad: EMap[_, T, WEval.Context] =>
          def f[S](monad: EMap[S, T, WEval.Context]): Fut[T] =
            compile(monad.src).map(monad.func, monad.hint.exec)
          f(monad)
        case monad: EFMap[_, T, WEval.Context] =>
          def f[S](monad: EFMap[S, T, WEval.Context]): Fut[T] =
            compile(monad.src).flatMap(s => compile(monad.func(s)))
          f(monad)
        case EFilter(src: Eval[T, WEval.Context], _, _) => compile(src)
        case chunkEval: WEvalChunk => other.invoke(GetChunk(chunkEval.p)).asInstanceOf[Fut[T]]
        case terrEval: WEvalTerrain => other.invoke(GetTerrain(terrEval.p)).asInstanceOf[Fut[T]]
        case entEval: WEvalEnt[_] => other.invoke(GetEnt(entEval.id)).asInstanceOf[Fut[T]]
      }
      accum.put(IdentityKey(monad), fut)
      fut
    })

  override def invoke[T](call: WEvalHelperService.RemoteCall[T],
                         other: LocalHelperServiceProcedureAccessor[WEvalHelperService]): Fut[T] = call match {
    case call: GetChunk => other.invoke(call).asInstanceOf[Fut[T]]
    case call: GetTerrain => other.invoke(call).asInstanceOf[Fut[T]]
    case call: GetEnt[_] => other.invoke(call).asInstanceOf[Fut[T]]
    case call: WEvalHelperService.Eval[_] => compile(call.eval)(new parallel.mutable.ParHashMap, other).asInstanceOf[Fut[T]]

  }

  override def close(): Unit = ()
}