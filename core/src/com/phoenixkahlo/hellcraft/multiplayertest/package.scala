package com.phoenixkahlo.hellcraft

import java.util.UUID

import com.phoenixkahlo.hellcraft.math.V3I

package object multiplayertest {

  type ClientID = UUID
  type EntityID = UUID
  type EventID = UUID
  type AvatarID = UUID

  type KryonetClient = com.esotericsoftware.kryonet.Client
  type KryonetServer = com.esotericsoftware.kryonet.Server

  val FakeLag = 0
  val TimeOut = Int.MaxValue

  val SubscribeDistance = V3I(5, 5, 5)
  val UpdateDistance = V3I(3, 3, 3)

}
