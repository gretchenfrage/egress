package com.phoenixkahlo.hellcraft

import java.lang.reflect.ParameterizedType
import java.util.UUID

import com.phoenixkahlo.hellcraft.gamedriver.Delta

import scala.concurrent.duration._
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.Env

import scala.reflect.ClassTag

package object singleplayer {

  val auxBackgroundThreads = 2
  val backgroundThreadPriority = 2
  val foregroundThreadPriority = 7
  val renderThreadPriority = 10

}