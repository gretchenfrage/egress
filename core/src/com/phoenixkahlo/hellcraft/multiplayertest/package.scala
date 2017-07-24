package com.phoenixkahlo.hellcraft

import java.util.UUID

/**
  * The server-client handshake:
  * - the client forms a connection to the server
  * - they exchange initial data packets
  * - they both use each other's packet to construct a local session object, and register it in an object space
  * - they send each other session-ready objects to signal that the session are ready for RMI
  * - it's showtime
  */
package object multiplayertest {

  type ClientID = UUID
  type EntityID = UUID
  type EventID = UUID
  type AvatarID = UUID

  val MinLag = 20
  val MaxLag = 20
  def randLag: Long = (Math.random() * (MaxLag - MinLag) + MinLag) toLong

}
