package com.phoenixkahlo.hellcraft.carbonite.egress

import com.phoenixkahlo.hellcraft.carbonite.{DefaultCarboniteConfig, NodeType}
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.{Cube, CubeFrame, SoundCube}
import com.phoenixkahlo.hellcraft.graphics.{DirtTID, GrassTID, SandTID, StoneTID}
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.fields._

object EgressCarboniteConfig extends DefaultCarboniteConfig {

  register[Chunk]()
  register[ProtoTerrain]()
  register[CompleteTerrain]()
  register[CompleteTerrain.Vert]()

  register[PutEntity]()
  register[RemoveEntity]()
  register[ShiftEntity]()

  register[CubeFrame]()
  register[Cube]()
  register[SoundCube]()

  register(Materials.getClass)

  register(Air.getClass)
  register(Stone.getClass)
  register(Dirt.getClass)

  register[V3F]()
  register[V3I]()
  register[V2F]()
  register[V2I]()
  register[V4F]()
  register(Origin.getClass)
  register(Up.getClass)
  register(Down.getClass)
  register(North.getClass)
  register(South.getClass)
  register(West.getClass)
  register(East.getClass)
  register(Origin2D.getClass)
  register(North2D.getClass)
  register(South2D.getClass)
  register(East2D.getClass)
  register(West2D.getClass)

  register(StoneTID.getClass)
  register(SandTID.getClass)
  register(DirtTID.getClass)
  register(GrassTID.getClass)

  register[Tri]()
  register[Quad]()

  register[ByteField]()
  register[ByteFractionField]()
  register[ShortField]()
  register[IDField[_]]()
  register[FloatField]()
  register[RefField[_]]()
  register[OptionField[_]]()

}
