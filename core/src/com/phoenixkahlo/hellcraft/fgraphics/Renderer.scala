package com.phoenixkahlo.hellcraft.fgraphics

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{GL20, PerspectiveCamera}
import com.phoenixkahlo.hellcraft.fgraphics.procedures._
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.math.{V3F, V4F}
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

import scala.reflect.ClassTag

trait Renderer {
  def apply(renders: Seq[Render[_]]): Unit
  def close(): Unit
}
/*
class DefaultRenderer(pack: ResourcePack) extends Renderer {
  val cam = new PerspectiveCamera(90, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
  cam.near = 0.1f
  cam.far = 3000
  cam.position.set(30, 30, 30)
  cam.lookAt(0, 25, 0)
  cam.up.set(0, 1, 0)

  val shaders: Map[ShaderTag[_], ShaderProcedure[_]] = {
    var map = Map.empty[ShaderTag[_], ShaderProcedure[_]]

    def reg[S <: Shader](procedure: ShaderProcedure[S])(implicit tag: ShaderTag[S]): Unit =
      map += tag -> procedure

    reg(new GenericShaderProcedure(pack))
    reg(new TerrainShaderProcedure(pack))
    reg(new ParticleShaderProcedure(pack))
    reg(new LineShaderProcedure)
    reg(new HUDShaderProcedure)

    map
  }
  def procedureFor[S <: Shader](shader: ShaderTag[S]): ShaderProcedure[S] = shaders(shader).asInstanceOf[ShaderProcedure[S]]

  val toFutPack = GEval.ToFutPack(UniExecutor.getService, pack)

  // final package contains cacheable results of renderables
  case class FinalPackage[S <: Shader](shader: ShaderTag[S], finalForm: S#FinalForm, translucentPos: Option[V3F])
  // here's a typed function that creates final packages asynchronously
  def _toFinalPackageFut[S <: Shader](renderable: Renderable[S]): Fut[FinalPackage[S]] = {
    renderable.eval.toFut(toFutPack)
      // notice that we perform this next mapping on the OpenGL thread
      .map[S#FinalForm](procedureFor(renderable.shader).toFinalForm, Gdx.app.postRunnable)
      .map[FinalPackage[S]](finalForm => FinalPackage(renderable.shader, finalForm, renderable.translucentPos))
  }
  // with a bit of type coercion, we turn this into an untyped memoizing function
  def toFinalPackageFut = new MemoFunc[Renderable[_], Fut[FinalPackage[_]]]((_toFinalPackageFut: Function1[_, _]).asInstanceOf[Any => Nothing])

  override def apply(renders: Seq[Render[_]], clearColor: V3F, lightPos: V3F): Unit = {
    // clear
    Gdx.gl.glClearColor(clearColor.x, clearColor.y, clearColor.z, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)

    // make globals
    val globals

    // find what we can render immediately
    case class InstantRenderable[S <: Shader](finalPackage: FinalPackage[S], params: S#Params)
    val units: Seq[InstantRenderable[_]] = {
      trait FakeAnyShader extends Shader {
        override type RenderUnit = Any
        override type Params = Any
      }
      // here we do some more type coercion
      renders.flatMap(render => toFinalPackageFut(render.renderable).query
        .map(finalPack => InstantRenderable[FakeAnyShader](finalPack.asInstanceOf[FinalPackage[FakeAnyShader]], render.params)))
    }

    // sequence (for translucency stuff)
    val (trans, opaqu) = units.partition(_.finalPackage.translucentPos.isDefined)
    val


  }

  override def close(): Unit = ???
}
*/