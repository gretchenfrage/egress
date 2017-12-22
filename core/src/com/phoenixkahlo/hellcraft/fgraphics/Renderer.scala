package com.phoenixkahlo.hellcraft.fgraphics

import java.util
import java.util.concurrent.{ConcurrentLinkedDeque, ConcurrentLinkedQueue, LinkedBlockingDeque, LinkedBlockingQueue}
import java.util.concurrent.atomic.AtomicBoolean

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.g3d.utils.{DefaultTextureBinder, RenderContext}
import com.badlogic.gdx.graphics.{Camera, GL20, PerspectiveCamera}
import com.phoenixkahlo.hellcraft.ShaderTag
import com.phoenixkahlo.hellcraft.core.eval.{Eval, ExecCheap, GEval}
import com.phoenixkahlo.hellcraft.core.eval.GEval.{CamRange, GEval, GLMap}
import com.phoenixkahlo.hellcraft.fgraphics.procedures._
import com.phoenixkahlo.hellcraft.math.{V2F, V3F, V4F}
import com.phoenixkahlo.hellcraft.util.collections._
import com.phoenixkahlo.hellcraft.util.debugging.Profiler
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

import scala.annotation.tailrec
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
  val procedures = new ShaderTagTable[ShaderProcedure]
  procedures += new GenericShaderProcedure(pack)
  procedures += new TerrainShaderProcedure(pack)
  procedures += new ParticleShaderProcedure(pack)
  procedures += new LineShaderProcedure
  procedures += new HUDShaderProcedure

  case class Prepared[S <: Shader](shader: ShaderTag[S], unit: S#FinalForm, translucentPos: Option[V3F])
  type PreparedEval[S <: Shader] = GEval[Prepared[S]]
  case class PrepareInput[S <: Shader](eval: GEval[S#RenderUnit], shader: ShaderTag[S], translucentPos: Option[V3F])
  val prepare: GenFunc[Renderable, PreparedEval, Shader] = new GenMemoFunc[Renderable, PreparedEval, Shader] {
    override protected def gen[S <: Shader](ren: Renderable[S]) =
      GLMap[S#RenderUnit, S#FinalForm](ren.eval, procedures(ren.shader).toFinalForm)
      .map[Prepared[S]]((ff: S#FinalForm) => Prepared(ren.shader, ff, ren.translucentPos))(ExecCheap)
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
    val screenRes = V2F(Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    val camRange = CamRange(cam.near, cam.far)
    val toFutPack = GEval.ToFutPack(UniExecutor.getService, pack, execOpenGL, screenRes, camRange)
    val evalNowPack = GEval.EvalNowPack(pack, screenRes, camRange)

    def extract[S <: Shader](render: Render[S], strong: Boolean = true): Option[RenderNow[_ <: Shader]] = {
      val eval: GEval[Prepared[S]] = render.renderable.pin match {
        case eval: Eval[_, _] => eval.asInstanceOf[GEval[Prepared[S]]]
        case _ =>
          val eval = prepare(render.renderable)
          render.renderable.pin = eval
          eval
      }
      val option: Option[Prepared[S]] =
        if (render.mustRender) eval.evalNow(evalNowPack)
        else {
          if (strong) eval.toFut(toFutPack).query
          else {
            println("doing weak fut query")
            val w = eval.weakFutQuery(toFutPack)
            println("wfq == " + w)
            w
          }
        }
        //eval.toFut(toFutPack).query
      option match {
        case Some(prepared) => Some(RenderNow(prepared, render.params, procedures(render.renderable.shader)))
        case None => render.deupdate.flatMap(extract(_, false))
          /*
          if (renderAlt) render.alt match {
            case Some(alt) => extract(alt, renderAlt = false)
            case None => None
          } else None
          */
      }
      //option.map(prepared => RenderNow(prepared, render.params, procedures(render.renderable.shader)))
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
    if (p.printDisc(16)) {
      println("render unit count: " + renders.size)
    }
  }

  override def onResize(width: Int, height: Int): Unit = {
    cam.viewportWidth = width
    cam.viewportHeight = height
    cam.update()
  }

  override def close(): Unit = {
    // just dispose of all the procedures
    for (procedure <- procedures.toSeq) {
      procedure.close()
    }
  }
}