package com.phoenixkahlo.hellcraft

import java.util.UUID

import com.phoenixkahlo.hellcraft.gamedriver.Delta

import scala.concurrent.duration._
import com.phoenixkahlo.hellcraft.math.V3I

package object singleplayer {

  type ClientID = UUID
  type EntityID = UUID
  type EventID = UUID
  type AvatarID = UUID

  val LoadDist = V3I(6, 6, 6)

  val DayCycleTime = 20 seconds
  val DayCycleTicks = DayCycleTime.toSeconds * Delta.updatesPerSecond
  val ShadowPixelDensity = 10

}
