package com.phoenixkahlo.hellcraft.multiplayertest

trait Transmission

case class InitialClientData() extends Transmission

case class ClientSessionReady(sessionID: Int) extends Transmission

case class InitialServerData(clientID: ClientID) extends Transmission

case class ServerSessionReady(sessionID: Int) extends Transmission

