package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.badlogic.gdx.graphics.{Color, GL20}
import com.badlogic.gdx.graphics.g3d.Renderable
import com.phoenixkahlo.hellcraft.core.event.{Events, UE}
import com.phoenixkahlo.hellcraft.core.graphics.{FreeCube, FreeCubeParams, RenderWorld}
import com.phoenixkahlo.hellcraft.core.{Event, SoundEffect}
import com.phoenixkahlo.hellcraft.fgraphics._
import com.phoenixkahlo.hellcraft.gamedriver.Delta
import com.phoenixkahlo.hellcraft.math.{Down, MRNG, V3F, V4I}
import com.phoenixkahlo.hellcraft.util.caches.ParamCache

abstract class Cube[E <: Cube[E]] extends Entity[E] {
  this: E =>

  def color: Color = Color.WHITE

  def tid: SheetTextureID

  def pos: V3F

  def lastPos: V3F = pos

  override def render(world: RenderWorld): Seq[Render[_ <: Shader]] = Seq(Render[GenericShader](
    FreeCube(FreeCubeParams(tid, V4I.ones)),
    Offset(pos + ((lastPos - pos) * world.interp))
  ))
}

case class SimpleCube(override val tid: SheetTextureID, override val pos: V3F,
                      override val id: EntID[SimpleCube]) extends Cube[SimpleCube]
object SimpleCube {
  def apply(tid: SheetTextureID, pos: V3F)(implicit rand: MRNG) =
    new SimpleCube(tid, pos, EntID())
}

case class SoundCube(sid: SoundID, freq: Int, override val pos: V3F,
                     override val id: EntID[SoundCube]) extends Cube[SoundCube] {
  override def tid = SoundTID

  override def update =
    Seq(Event(UE.time.map(time =>
      if (time % freq == 0) Seq(SoundEffect(sid, 1, pos))
      else Seq.empty
    )))
}
object SoundCube {
  def apply(sid: SoundID, freq: Int, pos: V3F)(implicit rand: MRNG) =
    new SoundCube(sid, freq, pos, EntID())
}

case class GlideCube(vel: V3F, override val pos: V3F,
                     override val id: EntID[GlideCube]) extends Cube[GlideCube] with Moveable[GlideCube]{
  override def tid = BrickTID
  override def updatePos(newPos: V3F) = copy(pos = newPos)
  override def update = Seq(Events.shift(id, vel * Delta.dtf))
}
object GlideCube {
  def apply(vel: V3F, pos: V3F)(implicit rand: MRNG) =
    new GlideCube(vel, pos, EntID())
}

case class GhostCube(override val pos: V3F, override val id: EntID[GhostCube])extends Cube[GhostCube] {
  override def tid = GrayTID
  override def color = new Color(1, 1, 1, 0.4f)
}
object GhostCube {
  def apply(pos: V3F)(implicit rand: MRNG) =
    new GhostCube(pos, EntID())
}
/*
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
}*/