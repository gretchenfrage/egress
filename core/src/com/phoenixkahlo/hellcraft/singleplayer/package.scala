package com.phoenixkahlo.hellcraft

import java.lang.reflect.ParameterizedType
import java.util.UUID

import com.phoenixkahlo.hellcraft.gamedriver.Delta

import scala.concurrent.duration._
import com.phoenixkahlo.hellcraft.math.V3I

import scala.reflect.ClassTag

package object singleplayer {

  type ClientID = UUID
  type EntityID = UUID
  type EventID = UUID
  type AvatarID = UUID

  val WorldRes = 8
  val LoadDist = V3I(6, 3, 6)

  val DayCycleTime = 20 seconds
  val DayCycleTicks = DayCycleTime.toSeconds * Delta.updatesPerSecond
  val ShadowPixelDensity = 10

  val mainLoopThreadPriority = 3
  val renderLoopThreadPriority = 3
  val backgroundThreadPriority = 1
  val useParCollections = false

}