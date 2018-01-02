package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.badlogic.gdx.graphics.{Color, GL20}
import com.badlogic.gdx.graphics.g3d.Renderable
import com.phoenixkahlo.hellcraft.core.graphics.{FreeCube, FreeCubeParams}
import com.phoenixkahlo.hellcraft.core.{ChunkEvent, RenderWorld, SoundEffect, UpdateEffect, World}
import com.phoenixkahlo.hellcraft.fgraphics._
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.math.{Down, MRNG, V3F, V4I}
import com.phoenixkahlo.hellcraft.util.caches.ParamCache

class Cube(tid: SheetTextureID, override val pos: V3F, override val id: UUID) extends Entity {
  protected def color: Color = Color.WHITE

  def lastPos: V3F = pos

  override def render(world: RenderWorld): Seq[Render[_ <: Shader]] = Seq(Render[GenericShader](
    FreeCube(FreeCubeParams(tid, V4I.ones)),
    Offset(pos + ((lastPos - pos) * world.interp))
  ))
}

case class SoundCube(sid: SoundID, freq: Int, override val pos: V3F, override val id: UUID) extends Cube(SoundTID, pos, id) {
  override def update(world: World)(implicit rand: MRNG): Seq[UpdateEffect] =
    if (world.time % freq == 0) Seq(SoundEffect(sid, 1, pos))
    else Seq.empty
}

case class GlideCube(vel: V3F, override val pos: V3F, override val id: UUID) extends Cube(GrassTID, pos, id) with Moveable {
  override def update(world: World)(implicit rand: MRNG): Seq[UpdateEffect] = {
    Seq(ChunkEvent.shift(chunkPos, id, vel * Delta.dtf))
  }

  override def updatePos(newPos: V3F): Moveable = copy(pos = newPos)
}

case class GhostCube(override val pos: V3F, override val id: UUID) extends Cube(GrayTID, pos, id) {
  override protected val color = new Color(1, 1, 1, 0.4f)
}