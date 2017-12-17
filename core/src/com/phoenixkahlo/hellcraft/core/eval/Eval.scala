package com.phoenixkahlo.hellcraft.core.eval

import com.badlogic.gdx.graphics.Pixmap.Format
import com.badlogic.gdx.graphics.{Pixmap, Texture}
import com.phoenixkahlo.hellcraft.core.{Chunk, Terrain}
import com.phoenixkahlo.hellcraft.core.eval.Eval.{CriticalData, EFlatMap, EMap}
import com.phoenixkahlo.hellcraft.core.request.ExecHint
import com.phoenixkahlo.hellcraft.fgraphics.ResourcePack
import com.phoenixkahlo.hellcraft.math.{V2F, V3I, V4F}
import com.phoenixkahlo.hellcraft.util.caches.ParamCache
import com.phoenixkahlo.hellcraft.util.collections.{MemoFunc, MemoHintFunc, SingleMemoFunc, SingleMemoHintFunc}
import com.phoenixkahlo.hellcraft.util.threading._

trait UniExecProvider {
  def executor: UniExecutor
}
trait EvalContext {
  type ToFutPack <: UniExecProvider
  type EvalNowPack
}

trait Eval[+T, E <: EvalContext] {
  def map[R](func: T => R)(implicit exec: ExecHint): Eval[R, E] =
    EMap(this, func, exec)
  def flatMap[R](func: T => Eval[R, E]): Eval[R, E] =
    EFlatMap(this, func)

  def toFut(pack: E#ToFutPack): Fut[T]
  def evalNow(pack: E#EvalNowPack): Option[T]
  def futCriticalData(pack: E#ToFutPack): CriticalData
  def evalCriticalData(pack: E#EvalNowPack): CriticalData
}
object Eval {
  def apply[T, E <: EvalContext](gen: => T)(implicit exec: ExecHint): Eval[T, E] =
    ECreate(() => gen, exec)
  def merge[A, B, T, E <: EvalContext](a: Eval[A, E], b: Eval[B, E], func: (A, B) => T)(implicit exec: ExecHint): Eval[T, E] =
    EMerge(a, b, func, exec)

  private [eval] type CriticalData = Any

  private case class ECreate[T, E <: EvalContext](factory: () => T, exec: ExecHint) extends Eval[T, E] {
    @transient private lazy val _toFut = new SingleMemoFunc[UniExecutor, Fut[T]](service => Fut(factory(), exec.exec(_)(service)))
    override def toFut(pack: E#ToFutPack): Fut[T] = _toFut(pack.executor)

    @transient private lazy val _evalNow = Some(factory())
    override def evalNow(pack: E#EvalNowPack): Option[T] = _evalNow

    override def futCriticalData(pack: E#ToFutPack): CriticalData = pack.executor
    override def evalCriticalData(pack: E#EvalNowPack): CriticalData = ()
  }

  private case class EMap[S, R, E <: EvalContext](source: Eval[S, E], func: S => R, exec: ExecHint) extends Eval[R, E] {
    @transient private lazy val _toFut = new SingleMemoHintFunc[(CriticalData, UniExecutor), E#ToFutPack, Fut[R]](
      (cs, pack) => cs match {
        case (crits, service) => source.toFut(pack).map(func, exec.exec(_)(service))
      })
    override def toFut(pack: E#ToFutPack): Fut[R] = _toFut((source.futCriticalData(pack), pack.executor), pack)

    @transient private lazy val _evalNow = new SingleMemoHintFunc[CriticalData, E#EvalNowPack, Option[R]](
      (crits, pack) => source.evalNow(pack).map(func)
    )
    override def evalNow(pack: E#EvalNowPack): Option[R] = _evalNow(source.evalCriticalData(pack), pack)

    override def futCriticalData(pack: E#ToFutPack): CriticalData = (pack.executor, source.futCriticalData(pack))
    override def evalCriticalData(pack: E#EvalNowPack): CriticalData = source.evalCriticalData(pack)
  }

  private case class EFlatMap[S, R, E <: EvalContext](source: Eval[S, E], func: S => Eval[R, E]) extends Eval[R, E] {
    @transient private lazy val _toFut = new SingleMemoHintFunc[(CriticalData, UniExecutor), E#ToFutPack, Fut[R]](
      (cs, pack) => cs match {
        case (crits, service) => source.toFut(pack).flatMap(func(_).toFut(pack))
      }
    )
    override def toFut(pack: E#ToFutPack): Fut[R] = _toFut((source.futCriticalData(pack), pack.executor), pack)

    @transient private lazy val _evalNow = new SingleMemoHintFunc[CriticalData, E#EvalNowPack, Option[R]](
      (crits, pack) => source.evalNow(pack).flatMap(func(_).evalNow(pack))
    )
    override def evalNow(pack: E#EvalNowPack): Option[R] = _evalNow(source.evalCriticalData(pack), pack)

    override def futCriticalData(pack: E#ToFutPack): CriticalData = (pack.executor, source.futCriticalData(pack))
    override def evalCriticalData(pack: E#EvalNowPack): CriticalData = source.evalCriticalData(pack)
  }

  private case class EMerge[A, B, R, E <: EvalContext](a: Eval[A, E], b: Eval[B, E], func: (A, B) => R, exec: ExecHint) extends Eval[R, E] {
    @transient private lazy val _toFut = new SingleMemoHintFunc[(CriticalData, CriticalData, UniExecutor), E#ToFutPack, Fut[R]](
      (crits, pack) => MergeFut(a.toFut(pack), b.toFut(pack), func(_, _))(exec.exec(_)(pack.executor))
    )
    override def toFut(pack: E#ToFutPack): Fut[R] = _toFut((a.futCriticalData(pack), b.futCriticalData(pack), pack.executor), pack)

    @transient private lazy val _evalNow = new SingleMemoHintFunc[(CriticalData, CriticalData), E#EvalNowPack, Option[R]](
      (crits, pack) => (a.evalNow(pack), b.evalNow(pack)) match {
        case (Some(aa), Some(bb)) => Some(func(aa, bb))
        case _ => None
      }
    )
    override def evalNow(pack: E#EvalNowPack): Option[R] = _evalNow((a.evalCriticalData(pack), b.evalCriticalData(pack)), pack)

    override def futCriticalData(pack: E#ToFutPack): CriticalData = (pack.executor, a.futCriticalData(pack), b.futCriticalData(pack))
    override def evalCriticalData(pack: E#EvalNowPack): CriticalData = (a.evalCriticalData(pack), b.evalCriticalData(pack))
  }
}

trait WEvalContext extends EvalContext {
  override type ToFutPack = WEval.ToFutPack
  override type EvalNowPack = WEval.EvalNowPack
}
object WEval {
  type WEval[T] = Eval[T, WEvalContext]

  def apply[T](gen: => T)(implicit exec: ExecHint): WEval[T] = Eval[T, WEvalContext](gen)

  case class ToFutPack(executor: UniExecutor, cfulfill: FulfillmentContext[V3I, Chunk], tfulfill: FulfillmentContext[V3I, Terrain])
  case class EvalNowPack(cfulfill: FulfillmentContext[V3I, Chunk], tfulfill: FulfillmentContext[V3I, Terrain])

  def chunk(p: V3I): WEval[Chunk] = EChunk(p)
  private case class EChunk(p: V3I) extends WEval[Chunk] {
    override def toFut(pack: ToFutPack): Fut[Chunk] =
      FulfillFut(p)(pack.cfulfill)

    override def evalNow(pack: EvalNowPack): Option[Chunk] =
      pack.cfulfill.get(p)

    override def futCriticalData(pack: ToFutPack): CriticalData = pack.cfulfill
    override def evalCriticalData(pack: EvalNowPack): CriticalData = pack.cfulfill
  }

  def terrain(p: V3I): WEval[Terrain] = ETerrain(p)
  private case class ETerrain(p: V3I) extends WEval[Terrain] {
    override def toFut(pack: ToFutPack): Fut[Terrain] =
      FulfillFut(p)(pack.tfulfill)

    override def evalNow(pack: EvalNowPack): Option[Terrain] =
      pack.tfulfill.get(p)

    override def futCriticalData(pack: ToFutPack): CriticalData = pack.tfulfill
    override def evalCriticalData(pack: EvalNowPack): CriticalData = pack.tfulfill
  }
}

trait GEvalContext extends EvalContext {
  override type ToFutPack = GEval.ToFutPack
  override type EvalNowPack = GEval.EvalNowPack
}
object GEval {
  type GEval[T] = Eval[T, GEvalContext]

  def apply[T](gen: => T)(implicit exec: ExecHint): GEval[T] = Eval[T, GEvalContext](gen)

  case class CamRange(near: Float, far: Float)
  case class ToFutPack(executor: UniExecutor, resourcePack: ResourcePack, glExec: Runnable => Unit, res: V2F, range: CamRange)
  case class EvalNowPack(resourcePack: ResourcePack, res: V2F, range: CamRange)

  val resourcePack: GEval[ResourcePack] = new GEval[ResourcePack] {
    override def toFut(pack: ToFutPack): Fut[ResourcePack] = Fut(pack.resourcePack, _.run())
    override def evalNow(pack: EvalNowPack): Option[ResourcePack] = Some(pack.resourcePack)
    override def futCriticalData(pack: ToFutPack): CriticalData = pack.resourcePack
    override def evalCriticalData(pack: EvalNowPack): CriticalData = pack.resourcePack
  }

  private val _dot = new MemoFunc[V4F, GEval[Texture]](col => new GEval[Texture] {
    @transient private lazy val _toFut = new ParamCache[Runnable => Unit, Fut[Texture]](glexec => Fut({
      val pixmap = new Pixmap(1, 1, Format.RGBA8888)
      pixmap.setColor(col.toColor)
      pixmap.drawPixel(0, 0)
      new Texture(pixmap)
    }, glexec))
    override def toFut(pack: ToFutPack): Fut[Texture] = _toFut(pack.glExec)

    @transient private lazy val _evalNow: Some[Texture] = {
      val pixmap = new Pixmap(1, 1, Format.RGBA8888)
      pixmap.setColor(col.toColor)
      pixmap.drawPixel(0, 0)
      Some(new Texture(pixmap))
    }
    override def evalNow(pack: EvalNowPack): Option[Texture] = _evalNow

    override def futCriticalData(pack: ToFutPack): CriticalData = ()
    override def evalCriticalData(pack: EvalNowPack): CriticalData = ()
  })

  val res: GEval[V2F] = new GEval[V2F] {
    override def toFut(pack: ToFutPack): Fut[V2F] = Fut(pack.res, _.run())
    override def evalNow(pack: EvalNowPack): Option[V2F] = Some(pack.res)
    override def futCriticalData(pack: ToFutPack): CriticalData = pack.res
    override def evalCriticalData(pack: EvalNowPack): CriticalData = pack.res
  }

  val camRange: GEval[CamRange] = new GEval[CamRange] {
    override def toFut(pack: ToFutPack): Fut[CamRange] = Fut(pack.range, _.run())
    override def evalNow(pack: EvalNowPack): Option[CamRange] = Some(pack.range)
    override def futCriticalData(pack: ToFutPack): CriticalData = pack.range
    override def evalCriticalData(pack: EvalNowPack): CriticalData = pack.range
  }
}