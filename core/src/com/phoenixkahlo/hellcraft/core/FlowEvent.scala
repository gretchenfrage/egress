package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.carbonite.CarboniteFields
import com.phoenixkahlo.hellcraft.math.{Origin, RNG, V3I}
import com.phoenixkahlo.hellcraft.util.fields.{ByteFractionField, FloatField}

case class FlowCatalyst(frequencies: List[V3I], override val target: V3I, override val id: UUID)
  extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = {
    var ids = RNG.uuids(RNG(id.getLeastSignificantBits))



    val densityShifts: Seq[ShiftDensity] = Seq.empty // TODO: implement
    if (frequencies.size > 1)
      (chunk,
        EffectDelayer(FlowCatalyst(frequencies.tail, target, ids.head), target, ids.drop(1).head) +: densityShifts)
    else (chunk, densityShifts)
  }
}

object Flow {
  def apply(p: V3I, id: UUID): FlowCatalyst =
    FlowCatalyst(Origin.until(V3I(3, 3, 3)).toList, p, id)
}

@CarboniteFields
case class ShiftDensity(delta: ByteFractionField, override val target: V3I, override val id: UUID)
extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = {
    val newDensities = FloatField(world.resVec, v => chunk.terrain.densities(v).get + delta(v).get)
    (chunk.updateTerrain(Densities(chunk.pos, chunk.terrain.materials, newDensities)), Seq.empty)
  }
}