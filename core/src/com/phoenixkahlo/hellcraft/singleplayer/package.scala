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

  val WorldRes = 16
  val LoadDist = V3I(8, 4, 8)

  val DayCycleTime = 20 seconds
  val DayCycleTicks = DayCycleTime.toSeconds * Delta.updatesPerSecond
  val ShadowPixelDensity = 20

  val auxBackgroundThreads = 4
  val mainLoopThreadPriority = 5
  val renderLoopThreadPriority = 10
  val backgroundThreadPriority = 3

}