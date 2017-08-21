package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.{ChunkEvent, World}
import com.phoenixkahlo.hellcraft.graphics.ResourcePack
import com.phoenixkahlo.hellcraft.graphics.`new`.RenderUnit
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}

trait Entity {

  def id: UUID

  def update(world: World, ids: Stream[UUID], dt: Float): Seq[ChunkEvent] = Seq.empty

  def renderables(pack: ResourcePack): Seq[RenderUnit] = Seq.empty

  def pos: V3F

  def chunkPos: V3I = pos / 16 floor

}
