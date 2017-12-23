package com.phoenixkahlo.hellcraft.core.eval

import java.nio.file.Path

import com.badlogic.gdx.files.FileHandle
import com.badlogic.gdx.graphics.Pixmap.Format
import com.badlogic.gdx.graphics.{Pixmap, Texture}
import com.phoenixkahlo.hellcraft.core.{Chunk, Terrain, TerrainGrid}
import com.phoenixkahlo.hellcraft.core.eval.Eval.{CriticalData, EFilter, EFlatMap, EMap}
import com.phoenixkahlo.hellcraft.fgraphics.ResourcePack
import com.phoenixkahlo.hellcraft.math.{V2F, V2I, V3I, V4F}
import com.phoenixkahlo.hellcraft.util.caches.{Cache, ParamCache}
import com.phoenixkahlo.hellcraft.util.collections.{MemoFunc, MemoHintFunc, SingleMemoFunc, SingleMemoHintFunc}
import com.phoenixkahlo.hellcraft.util.threading._

trait UniExecProvider {
  def executor: UniExecutor
}
trait EvalContext {
  type ToFutPack <: UniExecProvider
  type EvalNowPack
}

// wheeee, i'm a monad!
trait Eval[+T, E <: EvalContext] extends Serializable {
  def map[R](func: T => R)(implicit exec: ExecHint): Eval[R, E] =
    EMap(this, func, exec)
  def flatMap[R](func: T => Eval[R, E]): Eval[R, E] =
    EFlatMap(this, func)
  def filter(test: T => Boolean): Eval[T, E] =
    EFilter(this, test, ExecCheap)
  def withFilter(test: T => Boolean): Eval[T, E] =
    filter(test)
  def filter(test: T => Boolean, exec: ExecHint): Eval[T, E] =
    EFilter(this, test, exec)

  def toFut(pack: E#ToFutPack): Fut[T]
  def weakFutQuery(pack: E#ToFutPack): Option[T]
  def evalNow(pack: E#EvalNowPack): Option[T]
  def futCriticalData(pack: E#ToFutPack): CriticalData
  def evalCriticalData(pack: E#EvalNowPack): CriticalData
}
object Eval {
  def apply[T, E <: EvalContext](gen: => T)(implicit exec: ExecHint): Eval[T, E] =
    ECreate(() => gen, exec)
  def merge[A, B, T, E <: EvalContext](a: Eval[A, E], b: Eval[B, E])(func: (A, B) => T)(implicit exec: ExecHint): Eval[T, E] =
    EMerge(a, b, func, exec)

  private [eval] type CriticalData = Any

  private case class ECreate[T, E <: EvalContext](factory: () => T, exec: ExecHint) extends Eval[T, E] {
    @transient private lazy val _toFut = new SingleMemoFunc[UniExecutor, Fut[T]](service => Fut(factory(), exec.exec(_)(service)))
    override def toFut(pack: E#ToFutPack): Fut[T] = _toFut(pack.executor)
    override def weakFutQuery(pack: E#ToFutPack): Option[T] = _toFut.query(pack.executor).flatMap(_.query)

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
    override def weakFutQuery(pack: E#ToFutPack): Option[R] = _toFut.query((source.futCriticalData(pack), pack.executor)).flatMap(_.query)

    @transient private lazy val _evalNow = new SingleMemoHintFunc[CriticalData, E#EvalNowPack, Option[R]](
      (crits, pack) => source.evalNow(pack).map(func)
    )
    override def evalNow(pack: E#EvalNowPack): Option[R] = _evalNow(source.evalCriticalData(pack), pack)

    override def futCriticalData(pack: E#ToFutPack): CriticalData = (pack.executor, source.futCriticalData(pack))
    override def evalCriticalData(pack: E#EvalNowPack): CriticalData = source.evalCriticalData(pack)
  }

  private case class EFilter[T, E <: EvalContext](src: Eval[T, E], filter: T => Boolean, exec: ExecHint) extends Eval[T, E] {
    @transient private lazy val _toFut = new SingleMemoHintFunc[(CriticalData, UniExecutor), E#ToFutPack, Fut[T]](
      (cs, pack) => src.toFut(pack).filter(filter, exec.exec(_)(pack.executor))
    )
    override def toFut(pack: E#ToFutPack): Fut[T] = _toFut((src.futCriticalData(pack), pack.executor), pack)
    override def weakFutQuery(pack: E#ToFutPack): Option[T] = _toFut.query((src.futCriticalData(pack), pack.executor)).flatMap(_.query)

    @transient private lazy val _evalNow = new SingleMemoHintFunc[CriticalData, E#EvalNowPack, Option[T]](
      (crits, pack) => src.evalNow(pack).filter(filter)
    )
    override def evalNow(pack: E#EvalNowPack): Option[T] = _evalNow(src.evalCriticalData(pack), pack)

    override def futCriticalData(pack: E#ToFutPack): CriticalData = (pack.executor, src.futCriticalData(pack))
    override def evalCriticalData(pack: E#EvalNowPack): CriticalData = src.evalCriticalData(pack)
  }

  private case class EFlatMap[S, R, E <: EvalContext](source: Eval[S, E], func: S => Eval[R, E]) extends Eval[R, E] {
    @transient private lazy val _toFut = new SingleMemoHintFunc[(CriticalData, UniExecutor), E#ToFutPack, Fut[R]](
      (cs, pack) => cs match {
        case (crits, service) => source.toFut(pack).flatMap(func(_).toFut(pack))
      }
    )
    override def toFut(pack: E#ToFutPack): Fut[R] = _toFut((source.futCriticalData(pack), pack.executor), pack)
    override def weakFutQuery(pack: E#ToFutPack): Option[R] = _toFut.query((source.futCriticalData(pack), pack.executor)).flatMap(_.query)

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
    override def weakFutQuery(pack: E#ToFutPack): Option[R] = _toFut.query((a.futCriticalData(pack), b.futCriticalData(pack), pack.executor)).flatMap(_.query)

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
  type WEval[+T] = Eval[T, WEvalContext]

  def apply[T](gen: => T)(implicit exec: ExecHint): WEval[T] = Eval[T, WEvalContext](gen)

  case class ToFutPack(executor: UniExecutor, cfulfill: FulfillmentContext[V3I, Chunk], tfulfill: FulfillmentContext[V3I, Terrain]) extends UniExecProvider
  case class EvalNowPack(cfulfill: FulfillmentContext[V3I, Chunk], tfulfill: FulfillmentContext[V3I, Terrain])

  def chunk(p: V3I): WEval[Chunk] = EChunk(p)
  private case class EChunk(p: V3I) extends WEval[Chunk] {
    override def toFut(pack: ToFutPack): Fut[Chunk] =
      FulfillFut(p)(pack.cfulfill)

    override def evalNow(pack: EvalNowPack): Option[Chunk] =
      pack.cfulfill.get(p)

    override def weakFutQuery(pack: ToFutPack): Option[Chunk] =
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

    override def weakFutQuery(pack: ToFutPack): Option[Terrain] =
      pack.tfulfill.get(p)

    override def futCriticalData(pack: ToFutPack): CriticalData = pack.tfulfill
    override def evalCriticalData(pack: EvalNowPack): CriticalData = pack.tfulfill
  }

  private val emptyTerr = WEval(Map.empty[V3I, Terrain])(ExecCheap)
  def terrains(ps: Seq[V3I]): WEval[TerrainGrid] =
    ps.map(terrain)
      .map(_.map(terr => Map(terr.pos -> terr))(ExecCheap))
      .fold(emptyTerr)(Eval.merge(_, _)(_ ++ _)(ExecCheap))
      .map(TerrainGrid.fromMap)(ExecCheap)

}

trait GEvalContext extends EvalContext {
  override type ToFutPack = GEval.ToFutPack
  override type EvalNowPack = GEval.EvalNowPack
}
object GEval {
  type GEval[T] = Eval[T, GEvalContext]

  def apply[T](gen: => T)(implicit exec: ExecHint): GEval[T] = Eval[T, GEvalContext](gen)
  def readFile(handle: FileHandle): GEval[Either[Array[Byte], Throwable]] =
    FileRead(handle.file.toPath)

  case class CamRange(near: Float, far: Float)
  case class ToFutPack(executor: UniExecutor, resourcePack: ResourcePack, glExec: Runnable => Unit, res: V2I, range: CamRange) extends UniExecProvider
  case class EvalNowPack(resourcePack: ResourcePack, res: V2I, range: CamRange)

  val resourcePack: GEval[ResourcePack] = new GEval[ResourcePack] {
    override def toFut(pack: ToFutPack): Fut[ResourcePack] = Fut(pack.resourcePack, _.run())
    override def weakFutQuery(pack: ToFutPack) = Some(pack.resourcePack)
    override def evalNow(pack: EvalNowPack): Option[ResourcePack] = Some(pack.resourcePack)
    override def futCriticalData(pack: ToFutPack): CriticalData = pack.resourcePack
    override def evalCriticalData(pack: EvalNowPack): CriticalData = pack.resourcePack
  }

  def dot(col: V4F): GEval[Texture] = _dot(col)
  private val _dot = new MemoFunc[V4F, GEval[Texture]](col => new GEval[Texture] {
    @transient private lazy val _toFut = new ParamCache[Runnable => Unit, Fut[Texture]](glexec => Fut({
      val pixmap = new Pixmap(1, 1, Format.RGBA8888)
      pixmap.setColor(col.toColor)
      pixmap.drawPixel(0, 0)
      new Texture(pixmap)
    }, glexec))
    override def toFut(pack: ToFutPack): Fut[Texture] = _toFut(pack.glExec)
    override def weakFutQuery(pack: ToFutPack) = _toFut.query.flatMap(_.query)

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

  val res: GEval[V2I] = new GEval[V2I] {
    override def toFut(pack: ToFutPack): Fut[V2I] = Fut(pack.res, _.run())
    override def weakFutQuery(pack: ToFutPack) = Some(pack.res)
    override def evalNow(pack: EvalNowPack): Option[V2I] = Some(pack.res)
    override def futCriticalData(pack: ToFutPack): CriticalData = pack.res
    override def evalCriticalData(pack: EvalNowPack): CriticalData = pack.res
  }

  val camRange: GEval[CamRange] = new GEval[CamRange] {
    override def toFut(pack: ToFutPack): Fut[CamRange] = Fut(pack.range, _.run())
    override def weakFutQuery(pack: ToFutPack) = Some(pack.range)
    override def evalNow(pack: EvalNowPack): Option[CamRange] = Some(pack.range)
    override def futCriticalData(pack: ToFutPack): CriticalData = pack.range
    override def evalCriticalData(pack: EvalNowPack): CriticalData = pack.range
  }

  case class GLMap[S, R](source: GEval[S], func: S => R) extends GEval[R] {
    @transient private lazy val _toFut = new SingleMemoHintFunc[CriticalData, ToFutPack, Fut[R]](
      (cs, pack) => source.toFut(pack).map(func, pack.glExec)
    )
    override def toFut(pack: ToFutPack): Fut[R] = _toFut(source.futCriticalData(pack), pack)
    override def weakFutQuery(pack: ToFutPack): Option[R] = _toFut.query(source.futCriticalData(pack)).flatMap(_.query)

    @transient private lazy val _evalNow = new SingleMemoHintFunc[CriticalData, EvalNowPack, Option[R]](
      (cs, pack) => source.evalNow(pack).map(func)
    )
    override def evalNow(pack: EvalNowPack): Option[R] = _evalNow(source.evalCriticalData(pack), pack)

    override def futCriticalData(pack: ToFutPack): CriticalData = source.futCriticalData(pack)
    override def evalCriticalData(pack: EvalNowPack): CriticalData = source.evalCriticalData(pack)
  }

  private case class FileRead(path: Path) extends GEval[Either[Array[Byte], Throwable]] {
    @transient private lazy val fut = new Cache(new FileReadFut(path))
    override def toFut(pack: ToFutPack): Fut[Either[Array[Byte], Throwable]] = fut()
    override def weakFutQuery(pack: ToFutPack): Option[Either[Array[Byte], Throwable]] = fut.query.flatMap(_.query)

    override def evalNow(pack: EvalNowPack): Option[Either[Array[Byte], Throwable]] = Some(fut().await)
    override def futCriticalData(pack: ToFutPack): CriticalData = ()
    override def evalCriticalData(pack: EvalNowPack): CriticalData = ()
  }
}