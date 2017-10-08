package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.badlogic.gdx.graphics.GL20
import com.badlogic.gdx.graphics.g3d.Renderable
import com.phoenixkahlo.hellcraft.carbonite.CarboniteFields
import com.phoenixkahlo.hellcraft.core.{SoundEffect, UpdateEffect, UpdatePhysCubePosVel, World}
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.graphics.shaders.{GenericSID, TerrainSID}
import com.phoenixkahlo.hellcraft.math.{Down, V3F}
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


@CarboniteFields
case class SoundCube(sid: SoundID, freq: Int, override val pos: V3F, override val id: UUID) extends Cube(SoundTID, pos, id) {
  override def update(world: World, ids: Stream[UUID], dt: Float): Seq[UpdateEffect] =
    if (world.time % freq == 0) Seq(SoundEffect(sid, 1, pos))
    else Seq.empty
}

@CarboniteFields
case class PhysicsCube(override val pos: V3F, vel: V3F, override val id: UUID) extends Cube(SandTID, pos, id) {
  val barrier = 0.01f
  val maxVel = 100

  override def update(world: World, ids: Stream[UUID], dt: Float): Seq[UpdateEffect] = {
    val deltaT = Delta.dt.toMillis / 1000f

    var acceleratedVel = vel + (Down * 9.8f * deltaT)
    if (acceleratedVel.magnitude > maxVel)
      acceleratedVel = acceleratedVel.normalize * maxVel

    val suggestedDeltaPos = acceleratedVel * deltaT
    val suggestedNewPos = pos + suggestedDeltaPos
    val colliderNewPos = pos + suggestedDeltaPos.incrMag(barrier)

    world.seghit(pos, suggestedDeltaPos, suggestedDeltaPos.magnitude + barrier) match {
      case Some(hit) =>
        val barrierShift = (hit - colliderNewPos).normalize * barrier
        val resolvedNewPos = hit + barrierShift
        UpdatePhysCubePosVel(this, resolvedNewPos, acceleratedVel, ids)
      case None =>
        UpdatePhysCubePosVel(this, suggestedNewPos, acceleratedVel, ids)
    }

    /*
    // increment velocity and compute delta pos
    val deltaT = Delta.dt.toMillis / 1000f
    var newVel = vel + (Down * 9.8f * deltaT)
    if (newVel.magnitude > maxVel)
      newVel = newVel.normalize * maxVel
    val deltaPos = newVel * deltaT
    val incrPos = pos + deltaPos
    // compute collision, and branch to determine events
    world.seghit(pos, deltaPos, deltaPos.magnitude + barrier) match {
      case Some(hit) =>
        val barrierDelta = (hit - (incrPos.normalize * (incrPos.magnitude + barrier))).normalize * barrier
        val newPos = hit + barrierDelta
        println("final change in position: " + (newPos - pos))
        UpdatePhysCubePosVel(this, newPos, newVel, ids)
      case None =>
        UpdatePhysCubePosVel(this, pos + deltaPos, newVel, ids)
    }
    */
  }
}
