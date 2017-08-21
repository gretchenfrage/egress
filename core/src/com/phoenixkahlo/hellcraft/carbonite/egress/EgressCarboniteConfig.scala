package com.phoenixkahlo.hellcraft.carbonite.egress

import com.phoenixkahlo.hellcraft.carbonite.DefaultCarboniteConfig
import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.oldcore
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.fields.{ByteField, FractionField}

object EgressCarboniteConfig extends DefaultCarboniteConfig {

  register[oldcore.Chunk]()
  register[oldcore.BlockGrid]()

  register[oldcore.PutBlock]()
  register[oldcore.UncacheMesh]()
  register[oldcore.AddEntity]()
  register[oldcore.RemoveEntity]()
  register[oldcore.ReplaceEntity]()
  register[oldcore.SetAvatarMovement]()
  register[oldcore.ThrustCylindroid]()

  register[oldcore.entity.Avatar]()
  register[oldcore.entity.BlockOutline]()

  register[Chunk]()

  register[V3F]()
  register[V3I]()
  register[V2F]()
  register(Origin.getClass)
  register(Up.getClass)
  register(Down.getClass)
  register(North.getClass)
  register(South.getClass)
  register(East.getClass)
  register(West.getClass)

  register[ByteField]()
  register[FractionField]()

}
