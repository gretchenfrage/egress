package com.phoenixkahlo.hellcraft.carbonite.egress

import com.phoenixkahlo.hellcraft.carbonite.{DefaultCarboniteConfig, NodeType}
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.{Cube, CubeFrame}
import com.phoenixkahlo.hellcraft.graphics.StoneTID
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.fields.{ByteField, FractionField, OptionField}

object EgressCarboniteConfig extends DefaultCarboniteConfig {

  register[Chunk]()
  register[Densities]()
  register[Vertices]()
  register[Vertices.Vert]()
  register[Meshable]()

  register[AddEntity]()
  register[RemoveEntity]()
  register[ReplaceEntity]()
  register[SetCubePos]()

  register[CubeFrame]()
  register[Cube]()

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

  register[Tri]()
  register[Quad]()

  register[ByteField]()
  register[FractionField]()
  register[OptionField[_]]()

}
