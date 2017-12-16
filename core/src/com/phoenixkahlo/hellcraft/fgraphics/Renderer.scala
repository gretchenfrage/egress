package com.phoenixkahlo.hellcraft.fgraphics

import java.util
import java.util.concurrent.{ConcurrentLinkedDeque, ConcurrentLinkedQueue, LinkedBlockingDeque, LinkedBlockingQueue}
import java.util.concurrent.atomic.AtomicBoolean

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.g3d.utils.{DefaultTextureBinder, RenderContext}
import com.badlogic.gdx.graphics.{Camera, GL20, PerspectiveCamera}
import com.phoenixkahlo.hellcraft.fgraphics.procedures._
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.math.{V3F, V4F}
import com.phoenixkahlo.hellcraft.util.collections._
import com.phoenixkahlo.hellcraft.util.debugging.Profiler
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

import scala.reflect.ClassTag

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
  // task queue
  val tasks = new LinkedBlockingDeque[Either[Runnable, Unit]]
  def runTasks(): Unit = {
    var task: Either[Runnable, Unit] = null
    while ({ task = tasks.poll(); task != null })
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
  val procedures: TypeMatchingMap[ShaderTag, ShaderProcedure, Shader] = {
    var map = TypeMatchingMap.empty[ShaderTag, ShaderProcedure, Shader]
    def reg[S <: Shader](proc: ShaderProcedure[S])(implicit tag: ShaderTag[S]): Unit =
      map += (tag -> proc)

    reg(new GenericShaderProcedure(pack))
    reg(new TerrainShaderProcedure(pack))
    reg(new ParticleShaderProcedure(pack))
    reg(new LineShaderProcedure)
    reg(new HUDShaderProcedure)

    map
  }

  // input data to GEval
  val toFutPack = GEval.ToFutPack(UniExecutor.getService, pack, execOpenGL)
  // represents a renderable in form prepared to render
  case class Prepared[S <: Shader](shader: ShaderTag[S], unit: S#FinalForm, translucentPos: Option[V3F])
  type PreparedFut[S <: Shader] = Fut[Prepared[S]]
  // typesafe memoizing function from renderable to fut of prepared
  val prepare: GenFunc[Renderable, PreparedFut, Shader] = new GenMemoFunc[Renderable, PreparedFut, Shader] {
    override protected def gen[S <: Shader](renderable: Renderable[S]): Fut[Prepared[S]] = {
      renderable.eval.toFut(toFutPack)
        .map[S#FinalForm](procedures(renderable.shader).toFinalForm(_), execOpenGL _)
        .map[Prepared[S]]((finalForm: S#FinalForm) => Prepared(renderable.shader, finalForm, renderable.translucentPos))
    }
  }
  val immediate: GenFunc[Renderable, Prepared, Shader] = new GenMemoFunc[Renderable, Prepared, Shader] {
    override protected def gen[S <: Shader](renderable: Renderable[S]): Prepared[S] = {
      val finalForm = procedures(renderable.shader).toFinalForm(renderable.eval.eval(toFutPack))
      Prepared(renderable.shader, finalForm, renderable.translucentPos)
    }
  }

  // just a little fusion of a prepared renderable and params
  case class RenderNow[S <: Shader](prepared: Prepared[S], params: S#Params, procedure: ShaderProcedure[S])

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

    // find what we can render immediately
    def extract[S <: Shader](render: Render[S]): Option[RenderNow[S]] = {
      val option: Option[Prepared[S]] =
        if (render.mustRender) Some(immediate(render.renderable))
        else (render.renderable.pin match {
          case fut if fut.isInstanceOf[Fut[_]] => fut.asInstanceOf[PreparedFut[S]]
          case _ =>
            val prep = prepare(render.renderable)
            render.renderable.pin = prep
            prep
        }).query
      option.map(prepared => RenderNow(prepared, render.params, procedures(render.renderable.shader)))
    }
    // the compiler is struggling, this isn't haskell, so we're gonna have to help it out a little
    val renderNow: Seq[RenderNow[_ <: Shader]] = renders.flatMap((render: Render[_ <: Shader]) => extract(render): Option[RenderNow[_ <: Shader]])

    p.log()

    // sequence it
    // partition out sprite renders, those must be done at end
    val (isSprites, isntSprites) = renderNow.partition(_.procedure.isSprites)
    // partition again by translucency
    val (trans, opaqu) = isntSprites.partition(_.prepared.translucentPos.isDefined)
    // order translucent calls be distance to player
    val transSeq = trans.sortBy(_.prepared.translucentPos.get dist globals.camPos * -1)
    // cluster opaque calls by shader to minimize state changes
    val opaquSeq = opaqu.sortBy(_.prepared.shader.hashCode())
    // form an iterator for the render sequence
    val renderIter = opaquSeq.iterator ++ transSeq.iterator ++ isSprites.iterator

    p.log()

    // prepare to render
    var active: Option[ShaderProcedure[_ <: Shader]] = None
    def render[S <: Shader](rn: RenderNow[S]): Unit = {
      if (!active.contains(rn.procedure)) {
        if (active.isDefined) {
          active.get.end()
          context.end()
        }
        context.begin()
        active = Some(rn.procedure)
        active.get.begin(globals, context, cam)
      }
      rn.procedure(rn.prepared.unit, rn.params, globals, context, cam)
    }
    // ka-chow
    renderIter.foreach(render(_))
    // and before I go
    active.foreach(_.end())
    context.end()

    p.log()
    p.printDisc(16)
  }

  override def onResize(width: Int, height: Int): Unit = {
    cam.viewportWidth = width
    cam.viewportHeight = height
    cam.update()
  }

  override def close(): Unit = {
    // just dispose of all the procedures
    for (procedure <- procedures.toSeq.map(_._2)) {
      procedure.close()
    }
  }
}