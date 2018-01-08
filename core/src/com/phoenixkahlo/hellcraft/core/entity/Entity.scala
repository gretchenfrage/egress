package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.graphics.RenderWorld
import com.phoenixkahlo.hellcraft.core.UpdateEffect
import com.phoenixkahlo.hellcraft.fgraphics.{Render, Shader}
import com.phoenixkahlo.hellcraft.math.{MRNG, V3F, V3I}

trait Entity[E <: Entity[E]] extends Serializable {
  this: E =>

  def id: EntID[E]

  def update: Seq[UpdateEffect] = Seq.empty

  def render(world: RenderWorld): Seq[Render[_ <: Shader]]

  def pos: V3F

  def chunkPos: V3I = pos / 16 floor
}

trait Moveable[E <: Moveable[E]] extends Entity[E] {
  this: E =>

  def updatePos(newPos: V3F): E
}