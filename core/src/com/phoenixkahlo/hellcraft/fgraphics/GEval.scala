package com.phoenixkahlo.hellcraft.fgraphics


import com.badlogic.gdx.graphics.Pixmap.Format
import com.badlogic.gdx.graphics.{Color, Pixmap, Texture}
import com.phoenixkahlo.hellcraft.core.request.{Evalable, ExecHint}
import com.phoenixkahlo.hellcraft.fgraphics.GEval.ToFutPack
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.math.V4F
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc
import com.phoenixkahlo.hellcraft.util.threading.{Fut, MergeFut, UniExecutor}

trait GEval[+T] {
  def map[N](func: T => N)(implicit exec: ExecHint): GEval[N] =
    GEMap(this, func, exec)
  def flatMap[N](func: T => GEval[N]): GEval[N] =
    GEFlatMap(this, func)
  def toFut(pack: ToFutPack): Fut[T]
}
object GEval {
  case class ToFutPack(executor: UniExecutor, resourcePack: ResourcePack, glExec: Runnable => Unit)

  def apply[T](gen: => T)(implicit exec: ExecHint): GEval[T] =
    GECreate(() => gen, exec)
  def merge[A, B, N](a: GEval[A], b: GEval[B], func: (A, B) => N)(implicit exec: ExecHint): GEval[N] =
    GEMerge(a, b, func, exec)

  val resourcePack: GEval[ResourcePack] = new GEval[ResourcePack] {
    private val _toFut = new MemoFunc[ToFutPack, Fut[ResourcePack]](pack => Fut(pack.resourcePack, _.run()))
    override def toFut(pack: ToFutPack): Fut[ResourcePack] = _toFut(pack)
  }


  private val _dot = new MemoFunc[Color, GEval[Texture]](col => new GEval[Texture] {
    override def toFut(pack: ToFutPack): Fut[Texture] = Fut({
      val pixmap = new Pixmap(1, 1, Format.RGBA8888)
      pixmap.setColor(col)
      pixmap.drawPixel(0, 0)
      new Texture(pixmap)
    }, pack.glExec)
  })
  def dot(color: Color): GEval[Texture] = _dot(color)
  /*
  val _dot = new MemoFunc[Color, Texture](col => {
    val pixmap = new Pixmap(1, 1, Format.RGBA8888)
    pixmap.setColor(col)
    pixmap.drawPixel(0, 0)
    new Texture(pixmap)
  })

  override def dot(color: Color): Texture = _dot(color)
  */
}

private case class GECreate[T](factory: () => T, exec: ExecHint) extends GEval[T] {
  private val _toFut = new MemoFunc[ToFutPack, Fut[T]](pack => Fut(factory(), exec.exec(_)(pack.executor)))
  override def toFut(pack: ToFutPack): Fut[T] = _toFut(pack)
}
private case class GEMap[S, R](source: GEval[S], func: S => R, exec: ExecHint) extends GEval[R] {
  private val _toFut = new MemoFunc[ToFutPack, Fut[R]](pack => source.toFut(pack).map(func, exec.exec(_)(pack.executor)))
  override def toFut(pack: ToFutPack): Fut[R] = _toFut(pack)
}
private case class GEFlatMap[S, R](source: GEval[S], func: S => GEval[R]) extends GEval[R] {
  private val _toFut = new MemoFunc[ToFutPack, Fut[R]](pack => source.toFut(pack).flatMap(func(_).toFut(pack)))
  override def toFut(pack: ToFutPack): Fut[R] = _toFut(pack)
}
private case class GEMerge[A, B, R](a: GEval[A], b: GEval[B], func: (A, B) => R, exec: ExecHint) extends GEval[R] {
  private val _toFut = new MemoFunc[ToFutPack, Fut[R]](pack => MergeFut(a.toFut(pack), b.toFut(pack), func(_, _))(exec.exec(_)(pack.executor)))
  override def toFut(pack: ToFutPack): Fut[R] = _toFut(pack)
}