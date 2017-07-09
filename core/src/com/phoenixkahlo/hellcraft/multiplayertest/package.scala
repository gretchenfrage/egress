package com.phoenixkahlo.hellcraft

import java.util.UUID

/**
  * The server-client handshake:
  * - the client forms a connection to the server
  * - they exchange initial data packets
  * - they both use each other's packet to construct a local session object, and register it in an object space
  * - they send each other session-ready objects to signal that the session are ready for RMI
  * - as mr. schwarzenegger says, IT'S SHOWTIME
  */
package object multiplayertest {

  type ClientID = UUID
  type EntityID = UUID
  type EventID = UUID

}
