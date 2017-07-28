package com.phoenixkahlo.hellcraft.multiplayertest

/**
  * The server-client handshake:
  * - the client forms a connection to the server
  * - they exchange initial data packets
  * - they both use each other's packet to construct a local session object, and register it in an object space
  * - they send each other session-ready objects to signal that the session are ready for RMI
  * - it's showtime
  */
trait Transmission

case class InitialClientData() extends Transmission

case class ClientSessionReady(sessionID: Int) extends Transmission

case class InitialServerData(clientID: ClientID) extends Transmission

case class ServerSessionReady(sessionID: Int) extends Transmission

