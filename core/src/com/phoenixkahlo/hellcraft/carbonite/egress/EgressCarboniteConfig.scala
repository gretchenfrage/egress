package com.phoenixkahlo.hellcraft.carbonite.egress

import com.phoenixkahlo.hellcraft.carbonite.DefaultCarboniteConfig
import com.phoenixkahlo.hellcraft.oldcore._
import com.phoenixkahlo.hellcraft.oldcore.entity.{Avatar, BlockOutline}
import com.phoenixkahlo.hellcraft.math._

object EgressCarboniteConfig extends DefaultCarboniteConfig {

  register[Chunk]()
  register[BlockGrid]()

  register[PutBlock]()
  register[UncacheMesh]()
  register[AddEntity]()
  register[RemoveEntity]()
  register[ReplaceEntity]()
  register[SetAvatarMovement]()
  register[ThrustCylindroid]()

  register[Avatar]()
  register[BlockOutline]()

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

}
