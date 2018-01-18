package com.phoenixkahlo.hellcraft.fgraphics

import java.util
import java.util.UUID
import java.util.concurrent.{ConcurrentLinkedDeque, ConcurrentLinkedQueue, LinkedBlockingDeque, LinkedBlockingQueue}
import java.util.concurrent.atomic.AtomicBoolean

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.g3d.utils.{DefaultTextureBinder, RenderContext}
import com.badlogic.gdx.graphics.{Camera, GL20, PerspectiveCamera}
import com.phoenixkahlo.hellcraft.ShaderTag
import com.phoenixkahlo.hellcraft.core.eval._
import com.phoenixkahlo.hellcraft.core.eval.GEval.{CamRangeKey, GEval, ResKey, ResourcePackKey}
import com.phoenixkahlo.hellcraft.core.graphics.CamRange
import com.phoenixkahlo.hellcraft.fgraphics.procedures._
import com.phoenixkahlo.hellcraft.math.{V2F, V2I, V3F, V4F}
import com.phoenixkahlo.hellcraft.util.collections.ContextPin.{ContextPinFunc, ContextPinID}
import com.phoenixkahlo.hellcraft.util.collections._
import com.phoenixkahlo.hellcraft.util.debugging.Profiler
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}
import com.phoenixkahlo.hellcraft.util.time.Timer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.concurrent.duration._

trait Renderer {
  def apply(renders: Seq[Render[_ <: Shader]], globals: GlobalRenderData): Unit
  def cam: Camera
  def onResize(width: Int, height: Int): Unit
  def close(): Unit
}

/**
  * It was very hard to get the type system working with all of this.
  * TODO: dispose of old resources
  */
class DefaultRenderer(pack: ResourcePack) extends Renderer {
  // openGL task queue
  // the right unit singleton represents a special interruption value
  val tasks = new LinkedBlockingDeque[Either[Runnable, Unit]]
  val tickTaskLimit = 4 milliseconds
  def runTasks(limit: Duration = tickTaskLimit): Unit = {
    val timer = Timer.start
    var task: Either[Runnable, Unit] = null
    while (timer.elapsed < tickTaskLimit && { task = tasks.poll(); task != null })
      task.left.foreach(_.run())
  }
  def runTasksWhileAwaiting[T](fut: Fut[T]): T = {
    fut.onComplete(() => tasks.addFirst(Right((): Unit)))
    var continue = true
    while (continue) tasks.take() match {
      case Left(task) => task.run()
      case Right(()) => continue = false
    }
    fut.query.get
  }
  def execOpenGL(task: Runnable): Unit = {
    tasks.add(Left(task))
  }

  // resource management
  var resources: Fut[Set[EvalGraphs[_]]] = Fut(Set.empty, _.run())

  def register[T](seq: Seq[T])(implicit toGraph: T => EvalGraphs[_]): Unit = {
    resources = resources.map(_ ++ seq.map(toGraph).filter(_.isDisposable))
  }

  val cleanDuration = 2 seconds//10 seconds
  var misterClean: Timer = Timer.start

  def clean[T](seq: Seq[T])(implicit toGraph: T => EvalGraphs[_]): Unit = {
    val batch = 50

    def f(list: List[EvalGraphs[_]]): Fut[List[EvalGraphs[_]]] =
      if (list.isEmpty) Fut(Nil, _.run())
      else {
        val now = list.take(batch)
        val later = list.drop(batch)
        Fut[Unit]({
          //println("deleting " + now.size + " resources")
          for (graph <- now) {
            graph.async.dispose()
            graph.sync.dispose()
          }
        }, execOpenGL).flatMap(unit => f(later))
      }

    resources = resources.map(set => {
      val curr: Seq[EvalGraphs[_]] = seq.map(toGraph)
      val garbage: Set[EvalGraphs[_]] = set -- curr
      (curr.toSet, garbage.toList)
    }, UniExecutor.execc).flatMap({
      case (curr, garbage) => f(garbage).map({ case Nil => curr })
    })
  }

  def maybeClean[T](seq: Seq[T])(implicit toGraph: T => EvalGraphs[_]): Unit = {
    if (misterClean.elapsed >= cleanDuration) {
      clean(seq)
      misterClean = Timer.start
    }
  }

  // libgdx camera
  override val cam = new PerspectiveCamera(90, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
  cam.near = 0.1f
  cam.far = 3000
  cam.position.set(30, 30, 30)
  cam.lookAt(0, 25, 0)
  cam.up.set(0, 1, 0)

  // render context (makes OpenGL calls and minimizes state changes)
  val context = new RenderContext(new DefaultTextureBinder(DefaultTextureBinder.WEIGHTED, 1))

  // typesafe mapping from shader to shader procedure
  val procedures = new ShaderTagTable[ShaderProcedure]
  procedures += new GenericShaderProcedure(pack)
  procedures += new TerrainShaderProcedure(pack)
  procedures += new ParticleShaderProcedure(pack)
  procedures += new LineShaderProcedure
  procedures += new HUDShaderProcedure

  // typesafe pinned immediate renderable monad preperation
  type RNE[S <: Shader] = GEval[RenderableNow[S]]

  def prepareRaw[S <: Shader](renderable: Renderable[S]): RNE[S] =
    GEval.glMap[S#RenderUnit, S#FinalForm](renderable.eval, procedures(renderable.shader).toFinalForm)
      .map[RenderableNow[S]]((ff: S#FinalForm) => RenderableNow(renderable.shader, ff, renderable.transPos))(ExecCheap)

  val _prepareContextID: UUID = UUID.randomUUID()
  def prepare[S <: Shader](renderable: Renderable[S]): EvalGraphs[S] = {
    val pinFunc: ContextPinFunc[EvalGraphs[S]] = () => new EvalGraphs(renderable)
    val pinID: ContextPinID[EvalGraphs[S]] = _prepareContextID
    renderable.pin(pinID, pinFunc)
  }

  class EvalGraphs[S <: Shader](renderable: Renderable[S]) {
    val isDisposable: Boolean = procedures(renderable.shader).disposer.isDefined

    def dispose(r: RenderableNow[S]): Unit = {
      procedures(r.shader).disposer match {
        case Some(dispose) => dispose(r.unit)
        case None => //println("warning attempted to dispose of undisposable renderable")
      }
    }

    val rne: RNE[S] = prepareRaw(renderable)
    val sync: SyncEval[RenderableNow[S], GEval.Context] = new SyncEval(rne)(Some(dispose))
    val async: AsyncEval[RenderableNow[S], GEval.Context] = new AsyncEval(rne)(Some(_.map(dispose)))
  }

  // immediate rendering algebra
  case class RenderableNow[S <: Shader](shader: ShaderTag[S], unit: S#FinalForm, transPos: Option[V3F])
  case class RenderNow[S <: Shader](renderable: RenderableNow[S], params: S#Params)

  // cache of eval input map, we cache because identity improves state graph performance
  var evalIn: TypeMatchingMap[GEval.InKey, Identity, Any] = TypeMatchingMap.empty[GEval.InKey, Identity, Any]

  // eval async pack
  val toFutPack = GEval.EvalAsync(UniExecutor.getService, execOpenGL);

  // render a frame
  override def apply(renders: Seq[Render[_ <: Shader]], globals: GlobalRenderData): Unit = {
    val p = Profiler("render loop")

    // run tasks
    runTasks()

    p.log()

    // clear
    Gdx.gl.glClearColor(globals.clearCol.x, globals.clearCol.y, globals.clearCol.z, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)

    // move camera
    cam.position.set(globals.camPos.toGdx)
    cam.direction.set(globals.camDir.toGdx)
    cam.fieldOfView = globals.fov
    cam.update()

    p.log()

    // find what we can render immediately, and accumulate the graph buffer
    val screenRes = V2I(Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    val camRange = CamRange(cam.near, cam.far);
    {
      val newEvalIn = TypeMatchingMap[GEval.InKey, Identity, Any](
        ResourcePackKey -> pack,
        ResKey -> screenRes,
        CamRangeKey -> camRange
      )
      if (evalIn != newEvalIn)
        evalIn = newEvalIn
    }
    val extractedGraphs: mutable.Buffer[EvalGraphs[_]] = new mutable.ArrayBuffer[EvalGraphs[_]]
    def extract[S <: Shader](render: Render[S], strong: Boolean = true): Option[RenderNow[S]] = {
      val graphs = prepare(render.renderable)
      extractedGraphs += graphs
      val option =
        if (render.mustRender) graphs.sync.query(evalIn)
        else {
          if (strong) graphs.async.query(evalIn, toFutPack)
          else graphs.async.weakQuery(evalIn, toFutPack)
        }
      option match {
        case Some(renderable) => Some(RenderNow(renderable, render.params))
        case None => render.deupdate.flatMap(extract(_, false))/* match {
          case some@Some(_) => some
          case None =>
            //println("extraction failed")
            None
        }*/
      }
    }
    val renderNow: Seq[RenderNow[_ <: Shader]] = renders.flatMap((render: Render[_ <: Shader]) => extract(render): Option[RenderNow[_ <: Shader]])
    renderNow.foreach(rn => ()) // force evaluation if input is lazy, because of side effect requirements

    p.log()

    // memory management
    register(extractedGraphs)
    maybeClean(extractedGraphs)

    p.log()

    // sequence it
    // partition out sprite renders, those must be done at end
    val (isSprites, isntSprites) = renderNow.partition(ren => procedures(ren.renderable.shader).isSprites)
    // partition again by translucency
    val (trans, opaqu) = isntSprites.partition(_.renderable.transPos.isDefined)
    // order translucent calls by distance to player
    val transSeq = trans.sortBy(_.renderable.transPos.get dist globals.camPos * -1)
    // cluster opaque calls by shader to minimize state changes
    val opaquSeq: Seq[RenderNow[_ <: Shader]] = {
      type RenderNowBuffer[S <: Shader] = mutable.Buffer[RenderNow[S]]
      val bins = new ShaderTagTable[RenderNowBuffer]

      def prep[S <: Shader](implicit tag: ShaderTag[S]): Unit =
        bins += new ArrayBuffer[RenderNow[S]]
      ShaderTag.tags.foreach(prep(_))

      def add[S <: Shader](render: RenderNow[S]): Unit =
        bins(render.renderable.shader) += render
      opaqu.foreach(add(_))

      bins.toSeq.flatten
    }
    // form an iterator for the render sequence
    val renderIter = opaquSeq.iterator ++ transSeq.iterator ++ isSprites.iterator

    p.log()

    // prepare to render
    var active: Option[ShaderProcedure[_ <: Shader]] = None
    def render[S <: Shader](rn: RenderNow[S]): Unit = {
      val procedure = procedures(rn.renderable.shader)
      if (!active.contains(procedure)) {
        if (active.isDefined) {
          active.get.end()
          context.end()
        }
        context.begin()
        active = Some(procedure)
        procedure.begin(globals, context, cam)
      }
      procedure(rn.renderable.unit, rn.params, globals, context, cam)
    }
    // ka-chow
    renderIter.foreach(render(_))
    // and before I go
    active.foreach(_.end())
    context.end()

    p.log()
    p.printDisc(1000 / 60)
  }

  override def onResize(width: Int, height: Int): Unit = {
    cam.viewportWidth = width
    cam.viewportHeight = height
    cam.update()
  }

  override def close(): Unit = {
    for (procedure <- procedures.toSeq) {
      procedure.close()
    }
    clean(Seq.empty)
  }
}