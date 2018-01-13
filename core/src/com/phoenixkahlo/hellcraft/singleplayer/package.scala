package com.phoenixkahlo.hellcraft

import java.lang.reflect.ParameterizedType
import java.util.UUID

import com.phoenixkahlo.hellcraft.gamedriver.Delta

import scala.concurrent.duration._
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.Env

import scala.reflect.ClassTag

package object singleplayer {

  val auxBackgroundThreads = 0//Runtime.getRuntime.availableProcessors()//Env.physicalProcessors - 4
  val mainLoopThreadPriority = 5
  val renderLoopThreadPriority = 10
  val backgroundThreadPriority = 3

}