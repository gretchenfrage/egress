package com.phoenixkahlo.hellcraft

import java.util.UUID

package object multiplayertest {

  type ClientID = UUID
  type EntityID = UUID
  type EventID = UUID
  type AvatarID = UUID

  val FakeLag = 0
  val TimeOut = Int.MaxValue

}
