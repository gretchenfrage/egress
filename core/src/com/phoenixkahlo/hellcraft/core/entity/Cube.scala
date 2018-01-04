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

abstract class Cube[E <: Cube[E]](val tid: SheetTextureID, val pos: V3F) extends Entity[E] {
  this: E =>

  protected def color: Color = Color.WHITE

  def lastPos: V3F = pos

  override def render(world: RenderWorld): Seq[Render[_ <: Shader]] = Seq(Render[GenericShader](
    FreeCube(FreeCubeParams(tid, V4I.ones)),
    Offset(pos + ((lastPos - pos) * world.interp))
  ))
}

case class SimpleCube(override val tid: SheetTextureID, override val pos: V3F, override val id: EntID[SimpleCube]) extends Cube[SimpleCube](tid, pos)
object SimpleCube {
  def apply(tid: SheetTextureID, pos: V3F)(implicit rand: MRNG): SimpleCube =
    SimpleCube(tid, pos, EntID())
}

case class SoundCube(sid: SoundID, freq: Int, override val pos: V3F, override val id: EntID[SoundCube]) extends Cube[SoundCube](SoundTID, pos) {
  override def update(world: World)(implicit rand: MRNG): Seq[UpdateEffect] =
    if (world.time % freq == 0) Seq(SoundEffect(sid, 1, pos))
    else Seq.empty
}
object SoundCube {
  def apply(sid: SoundID, freq: Int, pos: V3F)(implicit rand: MRNG): SoundCube =
    SoundCube(sid, freq, pos, EntID())
}

case class GlideCube(vel: V3F, override val pos: V3F, override val id: EntID[GlideCube]) extends Cube[GlideCube](GrassTID, pos) with Moveable[GlideCube] {
  override def update(world: World)(implicit rand: MRNG): Seq[UpdateEffect] = {
    Seq(ChunkEvent.shift(chunkPos, id, vel * Delta.dtf))
  }

  override def updatePos(newPos: V3F): GlideCube = copy(pos = newPos)
}
object GlideCube {
  def apply(vel: V3F, pos: V3F)(implicit rand: MRNG): GlideCube =
    GlideCube(vel, pos, EntID())
}

case class GhostCube(override val pos: V3F, override val id: EntID[GhostCube]) extends Cube[GhostCube](GrayTID, pos) {
  override protected val color = new Color(1, 1, 1, 0.4f)
}
object GhostCube {
  def apply(pos: V3F)(implicit rand: MRNG): GhostCube =
    GhostCube(pos, EntID())
}