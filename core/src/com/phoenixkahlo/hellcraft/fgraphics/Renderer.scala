package com.phoenixkahlo.hellcraft.fgraphics

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.g3d.utils.{DefaultTextureBinder, RenderContext}
import com.badlogic.gdx.graphics.{GL20, PerspectiveCamera}
import com.phoenixkahlo.hellcraft.fgraphics.procedures._
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.math.{V3F, V4F}
import com.phoenixkahlo.hellcraft.util.collections.{GenFunc, GenMemoFunc, MemoFunc, TypeMatchingMap}
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

import scala.reflect.ClassTag

trait Renderer {
  def apply(renders: Seq[Render[_ <: Shader]], globals: GlobalRenderData): Unit
  def close(): Unit
}

/**
  * It was very hard to get the type system working with all of this.
  * TODO: dispose of old resources
  */
class DefaultRenderer(pack: ResourcePack) extends Renderer {
  // libgdx camera
  val cam = new PerspectiveCamera(90, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
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
  val toFutPack = GEval.ToFutPack(UniExecutor.getService, pack)
  // represents a renderable in form prepared to render
  case class Prepared[S <: Shader](shader: ShaderTag[S], unit: S#FinalForm, translucentPos: Option[V3F])
  type PreparedFut[S <: Shader] = Fut[Prepared[S]]
  // typesafe memoizing function from renderable to fut of prepared
  val prepare: GenFunc[Renderable, PreparedFut, Shader] = new GenMemoFunc[Renderable, PreparedFut, Shader] {
    override protected def gen[S <: Shader](renderable: Renderable[S]): Fut[Prepared[S]] =
      renderable.eval.toFut(toFutPack)
        .map[S#FinalForm](procedures(renderable.shader).toFinalForm(_), Gdx.app.postRunnable(_))
        .map[Prepared[S]]((finalForm: S#FinalForm) => Prepared(renderable.shader, finalForm, renderable.translucentPos))
  }

  // just a little fusion of a prepared renderable and params
  case class RenderNow[S <: Shader](prepared: Prepared[S], params: S#Params)

  override def apply(renders: Seq[Render[_ <: Shader]], globals: GlobalRenderData): Unit = {
    // clear
    Gdx.gl.glClearColor(globals.clearCol.x, globals.clearCol.y, globals.clearCol.z, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)

    // move camera
    cam.position.set(globals.camPos.toGdx)
    cam.direction.set(globals.camDir.toGdx)
    cam.fieldOfView = globals.fov
    cam.update()

    // find what we can render immediately
    def extract[S <: Shader](render: Render[S]): Option[RenderNow[S]] =
      prepare(render.renderable).query.map(prepared => RenderNow(prepared, render.params))
    // the compiler is struggling, this isn't haskell, so we're gonna have to help it out a little
    val renderNow: Seq[RenderNow[_ <: Shader]] = renders.flatMap((render: Render[_ <: Shader]) => extract(render): Option[RenderNow[_ <: Shader]])

    // sequence it
    val (trans, opaqu) = renderNow.partition(_.prepared.translucentPos.isDefined)
    // order the translucent calls by distance to player
    val transSeq = trans.sortBy(_.prepared.translucentPos.get dist globals.camPos * -1)
    // cluster the opaque calls by shader to minimize state changes
    val opaquSeq = opaqu.sortBy(_.prepared.shader.hashCode())
    // form a iterator for the render sequence
    val renderSeq = opaquSeq.iterator ++ transSeq.iterator

    // prepare to render
    var active: Option[ShaderProcedure[_ <: Shader]] = None
    def render[S <: Shader](rn: RenderNow[S]): Unit = {
      if (!active.map(_.shader).contains(rn.prepared.shader)) {
        active.foreach(_.end())
        active = Some(procedures(rn.prepared.shader))
        active.get.begin(globals, context, cam)
      }
      active.get.asInstanceOf[ShaderProcedure[S]].apply(rn.prepared.unit, rn.params, globals, context, cam)
    }
    // ka-chow!
    renderSeq.foreach(render(_))
  }

  override def close(): Unit = {
    // just dispose of all the procedures
    for (procedure <- procedures.toSeq.map(_._2)) {
      procedure.close()
    }
  }
}