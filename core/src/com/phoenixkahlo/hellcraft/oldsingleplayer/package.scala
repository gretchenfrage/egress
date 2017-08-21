package com.phoenixkahlo.hellcraft

import java.util.UUID

import com.phoenixkahlo.hellcraft.math.V3I

package object oldsingleplayer {

  type ClientID = UUID
  type EntityID = UUID
  type EventID = UUID
  type AvatarID = UUID

  val LoadDist = V3I(6, 6, 6)

}
