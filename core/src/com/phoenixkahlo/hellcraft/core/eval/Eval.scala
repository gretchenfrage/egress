package com.phoenixkahlo.hellcraft.core.eval

import java.io.{FileInputStream, IOException}
import java.nio.file.{Files, Path}

import com.badlogic.gdx.files.FileHandle
import com.badlogic.gdx.graphics.Pixmap.Format
import com.badlogic.gdx.graphics.{Pixmap, Texture}
import com.phoenixkahlo.hellcraft.core.entity.{AnyEnt, AnyEntID, EntID, Entity}
import com.phoenixkahlo.hellcraft.core.eval.Eval._
import com.phoenixkahlo.hellcraft.core.graphics.CamRange
import com.phoenixkahlo.hellcraft.core.{Chunk, Terrain, TerrainGrid}
import com.phoenixkahlo.hellcraft.fgraphics.ResourcePack
import com.phoenixkahlo.hellcraft.math.{V2F, V2I, V3I, V4F}
import com.phoenixkahlo.hellcraft.util.caches.{Cache, ParamCache}
import com.phoenixkahlo.hellcraft.util.collections._
import com.phoenixkahlo.hellcraft.util.threading._

trait UniExecProvider {
  def service: UniExecutor
}

trait Eval[+T, C <: Eval.Context] extends Serializable {
  def map[N](func: T => N)(implicit hint: ExecHint): Eval[N, C] = EMap(this, func, hint)
  def flatMap[N](func: T => Eval[N, C])(implicit hint: ExecHint): Eval[N, C] = EFMap(this, func, hint)
  def filter(test: T => Boolean)(implicit hint: ExecHint): Eval[T, C] = EFilter(this, test, hint)
  def withFilter(test: T => Boolean)(implicit hint: ExecHint): Eval[T, C] = filter(test)
}

object Eval {
  def apply[T, C <: Context](gen: => T)(implicit hint: ExecHint): Eval[T, C] = ECreate(() => gen, hint)

  def merge[A, B, T, C <: Context](a: Eval[A, C], b: Eval[B, C])(func: (A, B) => T)(implicit hint: ExecHint): Eval[T, C] =
    a.flatMap(aa => b.map(bb => func(aa, bb)))

  trait Context {
    type InKey[+T]
    type EvalAsync <: UniExecProvider
    type EvalSync
  }
  case class ECreate[T, C <: Context](fac: () => T, hint: ExecHint) extends Eval[T, C]
  case class EInput[T, C <: Context](key: C#InKey[T]) extends Eval[T, C]
  case class EMap[S, T, C <: Context](src: Eval[S, C], func: S => T, hint: ExecHint) extends Eval[T, C]
  case class EFMap[S, T, C <: Context](src: Eval[S, C], func: S => Eval[T, C], hint: ExecHint) extends Eval[T, C]
  case class EFilter[T, C <: Context](src: Eval[T, C], test: T => Boolean, hint: ExecHint) extends Eval[T, C]
  case class EExtern[T, C <: Context](sync: C#EvalSync => Option[T], async: C#EvalAsync => Fut[T], triggers: Seq[C#InKey[Any]]) extends Eval[T, C]
  case class ESpecialMap[S, T, C <: Context](src: Eval[S, C], func: S => T, exec: C#EvalAsync => (Runnable => Unit)) extends Eval[T, C]
}

object WEval {
  case class EvalAsync(service: UniExecutor, cfulfill: FulfillmentContext[V3I, Chunk], tfulfill: FulfillmentContext[V3I, Terrain], efulfill: FulfillmentContext[AnyEntID, AnyEnt]) extends UniExecProvider
  case class EvalSync(cfulfill: FulfillmentContext[V3I, Chunk], tfulfill: FulfillmentContext[V3I, Terrain], efulfill: FulfillmentContext[AnyEntID, AnyEnt])

  trait Context extends Eval.Context {
    override type InKey[+T] = Nothing
    override type EvalAsync = WEval.EvalAsync
    override type EvalSync = WEval.EvalSync
  }
  val input = TypeMatchingMap.empty[Context#InKey, Identity, Any]

  type WEval[+T] = Eval[T, Context]

  def apply[T](gen: => T)(implicit exec: ExecHint): WEval[T] = Eval[T, Context](gen)

  def chunk(p: V3I): WEval[Chunk] = EExtern[Chunk, Context](
    _.cfulfill.get(p),
    _.cfulfill.fut(p),
    Seq.empty
  )

  def terrain(p: V3I): WEval[Terrain] = EExtern[Terrain, Context](
    _.tfulfill.get(p),
    _.tfulfill.fut(p),
    Seq.empty
  )

  def ent[E <: Entity[E]](id: EntID[E]): WEval[E] = EExtern[E, Context](
    _.efulfill.get(id).asInstanceOf[Option[E]],
    _.efulfill.get(id).asInstanceOf[Fut[E]],
    Seq.empty
  )

  private val emptyTerr = WEval(Map.empty[V3I, Terrain])(ExecCheap)
  def terrains(ps: Seq[V3I]): WEval[TerrainGrid] =
    ps.map(terrain)
      .map(_.map(terr => Map(terr.pos -> terr))(ExecCheap))
      .fold(emptyTerr)(Eval.merge(_, _)(_ ++ _)(ExecCheap))
      .map(TerrainGrid)(ExecCheap)
}

object GEval {
  case class EvalAsync(service: UniExecutor, glExec: Runnable => Unit) extends UniExecProvider

  trait Context extends Eval.Context {
    override type InKey[+T] = GEval.InKey[T]
    override type EvalAsync = GEval.EvalAsync
    override type EvalSync = Unit
  }

  sealed trait InKey[+T]
  case object ResourcePackKey extends InKey[ResourcePack]
  case object ResKey extends InKey[V2I]
  case object CamRangeKey extends InKey[CamRange]

  type GEval[+T] = Eval[T, Context]

  def apply[T](gen: => T)(implicit exec: ExecHint): GEval[T] = Eval[T, Context](gen)

  val resourcePack: GEval[ResourcePack] = EInput[ResourcePack, Context](ResourcePackKey)

  val res: GEval[V2I] = EInput[V2I, Context](ResKey)

  val camRange: GEval[CamRange] = EInput[CamRange, Context](CamRangeKey)

  def dot(col: V4F): GEval[Texture] = _dot(col)
  private def genDot(col: V4F): Texture = {
    val pixmap = new Pixmap(1, 1, Format.RGBA8888)
    pixmap.setColor(col.toColor)
    pixmap.drawPixel(0, 0)
    new Texture(pixmap)
  }
  private val _dot = new MemoFunc[V4F, GEval[Texture]](col => EExtern[Texture, Context](
    unit => Some(genDot(col)),
    pack => Fut[Texture](genDot(col), pack.glExec),
    Seq.empty
  ))

  def glMap[S, R](src: GEval[S], func: S => R): GEval[R] =
    ESpecialMap(src, func, _.glExec)

  def readResource(path: FileHandle): GEval[Either[Array[Byte], Throwable]] = {
    def read(): Either[Array[Byte], Throwable] = {
      if (path.length > Int.MaxValue)
        Right(new IllegalArgumentException("file too large"))
      else try
        Left(path.readBytes())
      catch {
        case t: Throwable => Right(t)
      }
    }

    EExtern[Either[Array[Byte], Throwable], Context](
      unit => Some(read()),
      pack => Fut(read(), AsyncExecutor.global.execute),
      Seq.empty
    )
  }

  /*
  def readFile(path: Path): GEval[Either[Array[Byte], Throwable]] = EExtern[Either[Array[Byte], Throwable], Context](
    unit => {
      try Some(Left(Files.readAllBytes(path)))
      catch {
        case e: IOException => Some(Right(e))
      }
    },
    pack => new FileReadFut(path),
    Seq.empty
  )
  */
}
