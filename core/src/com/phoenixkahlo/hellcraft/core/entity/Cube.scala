package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.badlogic.gdx.graphics.GL20
import com.badlogic.gdx.graphics.g3d.Renderable
import com.phoenixkahlo.hellcraft.carbonite.CarboniteFields
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.graphics.shaders.{GenericSID, TerrainSID}
import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.util.ResourceNode
import com.phoenixkahlo.hellcraft.util.caches.ParamCache

@CarboniteFields
class Cube(tid: SheetTextureID, override val pos: V3F, override val id: UUID) extends Entity {
  @transient private lazy val renderUnit = new ParamCache[ResourcePack, Seq[RenderUnit]](pack => {
    val renderable = new Renderable
    renderable.meshPart.mesh = FreeCubeMesh(tid, (tid, pack))
    renderable.material = new com.badlogic.gdx.graphics.g3d.Material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = renderable.meshPart.mesh.getNumIndices
    renderable.meshPart.primitiveType = GL20.GL_TRIANGLES
    renderable.userData = GenericSID
    renderable.worldTransform.translate(pos toGdx)
    val renderableSeq = Seq(renderable)
    Seq(new RenderUnit {
      override def apply(interpolation: Interpolation): Seq[Renderable] = renderableSeq

      override def resources: Seq[ResourceNode] = Seq.empty
    })
  })

  override def renderables(pack: ResourcePack): Seq[RenderUnit] = renderUnit(pack)
}