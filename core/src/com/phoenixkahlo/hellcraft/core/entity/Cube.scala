package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.badlogic.gdx.graphics.{Color, GL20}
import com.badlogic.gdx.graphics.g3d.Renderable
import com.phoenixkahlo.hellcraft.carbonite.CarboniteFields
import com.phoenixkahlo.hellcraft.core.{Shift, SoundEffect, UpdateEffect, World}
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.graphics.models.FreeCubeMesh
import com.phoenixkahlo.hellcraft.graphics.shaders.{GenericSID, TerrainSID}
import com.phoenixkahlo.hellcraft.math.{Down, V3F}
import com.phoenixkahlo.hellcraft.util.caches.ParamCache
import com.phoenixkahlo.hellcraft.util.collections.ResourceNode

@CarboniteFields
class Cube(tid: SheetTextureID, override val pos: V3F, override val id: UUID) extends Entity {
  protected def color: Color = Color.WHITE

  @transient private lazy val renderUnit = new ParamCache[ResourcePack, Seq[RenderUnit]](pack => {
    val renderable = new Renderable
    renderable.meshPart.mesh = FreeCubeMesh((tid, color), (tid, pack, color))
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

      override def locationIfTransparent =
        if (color.a == 1) None
        else Some(pos)
    })
  })

  override def renderables(pack: ResourcePack): Seq[RenderUnit] = renderUnit(pack)
}

@CarboniteFields
case class SoundCube(sid: SoundID, freq: Int, override val pos: V3F, override val id: UUID) extends Cube(SoundTID, pos, id) {
  override def update(world: World, ids: Stream[UUID]): Seq[UpdateEffect] =
    if (world.time % freq == 0) Seq(SoundEffect(sid, 1, pos))
    else Seq.empty
}

@CarboniteFields
case class GlideCube(vel: V3F, override val pos: V3F, override val id: UUID) extends Cube(GrassTID, pos, id) with Moveable {
  override def update(world: World, ids: Stream[UUID]): Seq[UpdateEffect] = {
    Seq(Shift(vel * Delta.dtf, id, chunkPos, ids.head))
  }

  override def updatePos(newPos: V3F): Entity = copy(pos = newPos)
}

@CarboniteFields
case class GhostCube(override val pos: V3F, override val id: UUID) extends Cube(GrayTID, pos, id) {
  override protected val color = new Color(1, 1, 1, 0.4f)
}