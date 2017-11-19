package com.phoenixkahlo.hellcraft.fgraphics

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Mesh, PerspectiveCamera}
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.{Environment, Material, Renderable}
import com.phoenixkahlo.hellcraft.core.request.{ExecHint, ExecSeq}
import com.phoenixkahlo.hellcraft.graphics.{HUD, Interpolation, RenderUnit, ResourcePack}
import com.phoenixkahlo.hellcraft.graphics.shaders._
import com.phoenixkahlo.hellcraft.math.{V2F, V3F}
import com.phoenixkahlo.hellcraft.util.caches.{DisposableParamCache, ParamCache}
import com.phoenixkahlo.hellcraft.util.collections.{MemoHintFunc, ResourceNode}
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

class Render(verts: Seq[Float], indices: Seq[Short], shader: ShaderID) extends (ResourcePack => RenderUnit) {
  val renderable = new DisposableParamCache[ResourcePack, Renderable](pack => {
    val mesh = new Mesh(true, verts.size, indices.size, shader.attribs.toArray: _*)
    mesh.setVertices(verts.toArray)
    mesh.setIndices(indices.toArray)

    val renderable = new Renderable
    renderable.meshPart.mesh = mesh
    renderable.material = new Material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = indices.size
    renderable.meshPart.primitiveType = shader.primType
    renderable.userData = shader

    renderable
  }, _.meshPart.mesh.dispose())

  val unit = new ParamCache[ResourcePack, RenderUnit](pack =>
    new RenderUnit {
      override def apply(interpolation: Interpolation): Seq[Renderable] =
        Seq(renderable(pack))

      // it is essential that we declare this as a val, not a def
      override val resources: Seq[ResourceNode] =
        Seq(new ResourceNode {
          override def dependencies: Seq[ResourceNode] = Seq.empty

          override def dispose(): Unit = renderable.invalidate
        })
    })

  override def apply(pack: ResourcePack): RenderUnit = unit(pack)
}
object Render {
  def apply(verts: Seq[Float], indices: Seq[Short], shader: ShaderID): Render =
    new Render(verts, indices, shader)
}

case class MakeRender(factory: () => (ResourcePack => RenderUnit), execHint: ExecHint = ExecSeq)


class FRenderer(pack: ResourcePack)(implicit service: UniExecutor) {
  // TODO: make memo func soft and make futs weak
  val memoizer: MemoHintFunc[() => (ResourcePack => RenderUnit), ExecHint, Fut[ResourcePack => RenderUnit]] =
    new MemoHintFunc((factory, execHint) => Fut(factory(), execHint.exec))

  val environment = new Environment
  environment.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
  environment.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))

  val cam = new PerspectiveCamera(90, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
  cam.near = 0.1f
  cam.far = 3000
  cam.position.set(30, 30, 30)
  cam.lookAt(0, 25, 0)
  cam.up.set(0, 1, 0)

  val terrainShader = new TerrainShader(pack.sheet)
  terrainShader.init()
  val lineShader = new LineShader
  lineShader.init()
  val basicShader = new BasicShader(pack.sheet, TerrainSID)
  basicShader.init()
  val pointShader = new PointShader
  pointShader.init()
  val genericShader = new GenericShader(pack.sheet)
  genericShader.init()
  val particleShader = new ParticleShader(pack.sheet)
  particleShader.init()

  def render(make: Seq[MakeRender], hud: HUD): Unit = {
    val units: Seq[RenderUnit] =
      make.map(factory => memoizer(factory.factory, factory.execHint)).flatMap(_.query).map(_(pack))
  }
}
