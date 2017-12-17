package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.{ChunkEvent, RenderWorld, UpdateEffect, World}
import com.phoenixkahlo.hellcraft.fgraphics.{Render, Shader}
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}

trait Entity extends Serializable {
  def id: UUID

  def update(world: World, ids: Stream[UUID]): Seq[UpdateEffect] = Seq.empty

  def render(world: RenderWorld): Seq[Render[_ <: Shader]]

  def pos: V3F

  def chunkPos: V3I = pos / 16 floor
}

trait Moveable extends Entity {
  def updatePos(newPos: V3F): Entity
}