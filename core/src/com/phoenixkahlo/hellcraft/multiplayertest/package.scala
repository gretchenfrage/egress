package com.phoenixkahlo.hellcraft

import java.util.UUID

package object multiplayertest {

  type ClientID = UUID
  type EntityID = UUID
  type EventID = UUID
  type AvatarID = UUID

  type KryonetClient = com.esotericsoftware.kryonet.Client
  type KryonetServer = com.esotericsoftware.kryonet.Server

  val FakeLag = 1000
  val TimeOut = Int.MaxValue

}
