package com.phoenixkahlo.hellcraft.fgraphics

import com.phoenixkahlo.hellcraft.core.eval.Eval._
import com.phoenixkahlo.hellcraft.core.eval.{GEval, WEval}
import com.phoenixkahlo.hellcraft.core.eval.GEval.{CamRangeKey, GEval, ResKey, ResourcePackKey}
import com.phoenixkahlo.hellcraft.util.collections.{Identity, TypeMatchingMap}
import com.phoenixkahlo.hellcraft.util.retro.{Retro, SetRetro, WeaklyObservableRetro}
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}


class GEvalManager(implicit service: UniExecutor) {
  val settables: Map[GEval.InKey[_], SetRetro[_]] = Map(
    ResourcePackKey -> new SetRetro,
    ResKey -> new SetRetro,
    CamRangeKey -> new SetRetro
  )

  val observables: Map[GEval.InKey[_], WeaklyObservableRetro[_]] =
    settables.map({ case (k, v) => (k, new WeaklyObservableRetro(v)) })

  def setInput(in: TypeMatchingMap[GEval.InKey, Identity, Any]): Unit = {
    for ((k, set) <- settables)
      set.asInstanceOf[SetRetro[Any]].set(in.unsafeget(k))
  }

  /**
    * @param eo executor override, which's presence also enables synchronous evaluation mode for externs
    */
  def toRetro[T](eval: GEval[T])(implicit easync: WEval.EvalAsync, eo: Option[Runnable => Unit]): Retro[T] = eval match {
    case ECreate(fac, hint) => Retro(fac(), eo.getOrElse(hint.exec))
    case EInput(key: GEval.InKey[T]) => observables(key).asInstanceOf[Retro[T]]
    case eval: EMap[_, T, GEval.Context] =>
      def f[S](eval: EMap[S, T, GEval.Context]): Retro[T] =
        toRetro(eval.src).map(eval.func, eo.getOrElse(eval.hint.exec))
      f(eval)
    case eval: EFMap[_, T, GEval.Context] =>
      def f[S](eval: EFMap[S, T, GEval.Context]): Retro[T] =
        toRetro(eval.src).flatMap(eval.func.andThen(toRetro(_)))
      f(eval)
    case EFilter(src, _, _) => toRetro(src)
    case EExtern(sync: (Unit => Option[T]), async: (GEval.EvalAsync => Fut[T]), triggers: Seq[GEval.InKey[Any]]) =>
      triggers.map(observables)
        .map(_.map(any => (), _.run()))
        .reduce((r1: Retro[Unit], r2: Retro[Unit]) => r1.flatMap[Unit](any => r2))
        .flatMap(any =>
          if (eo.isDefined) sync(()).map(Retro(_, _.run())).getOrElse(new SetRetro)
          else Retro.fromFut(async(easync))
        )
    case eval: ESpecialMap[_, T, GEval.Context] =>
      def f[S](eval: ESpecialMap[S, T, GEval.Context]): Retro[T] =
        toRetro(eval.src).map(eval.func, eval.exec(easync))
      f(eval)
  }
}
