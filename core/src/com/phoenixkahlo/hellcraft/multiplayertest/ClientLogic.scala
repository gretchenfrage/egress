package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.UUID
import java.util.concurrent.{Executors, LinkedBlockingQueue, ThreadLocalRandom}

import com.esotericsoftware.kryonet.Connection
import com.esotericsoftware.kryonet.rmi.{ObjectSpace, RemoteObject}
import com.phoenixkahlo.hellcraft.core.{AddEntity, Chunk, ChunkEvent}
import com.phoenixkahlo.hellcraft.core.entity.Avatar
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}
import com.phoenixkahlo.hellcraft.util.{AsyncExecutor, LaggyNoReplyProxy, LaggyProxy}

import scala.collection.{SortedMap, SortedSet}
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

/**
  * Wraps a client session on the server side for serverside client-specific logic.
  *
  * It is ready to receive message upon construction, but initialize must be called to
  * perform the handshake.
  */
class ClientLogic(server: GameServer) {

  val clientID: ClientID = UUID.randomUUID()
  val received = new LinkedBlockingQueue[Any]

  def receive(message: Any): Unit = received.add(message)

  var seqExecutor: ExecutionContext = _
  var rmiSpace: ObjectSpace = _
  var avatarID: AvatarID = _

  var session: ClientSession = _
  var sessionNoReply: ClientSession = _

  def initialize(connection: Connection): Unit = {
    // configure the connection
    connection.setTimeout(TimeOut)
    // create the seq executor
    seqExecutor = ExecutionContext.fromExecutor(
      Executors.newSingleThreadExecutor(runnable => new Thread(runnable, "client seq thread")))
    // create the RMI space
    rmiSpace = new ObjectSpace
    rmiSpace.setExecutor(AsyncExecutor("server RMI thread"))
    rmiSpace.addConnection(connection)
    // create the avatar, we will integrate it at the end
    val avatar = Avatar(pos = V3F(0, 10, 0))
    avatarID = avatar.id
    // send our initial data
    connection.sendTCP(InitialServerData(clientID))
    // receive the client's initial data
    val init = received.take().asInstanceOf[InitialClientData]
    // use the initial data to create a session
    val serverSession = new ServerSessionImpl(server, this)
    val sessionID = ThreadLocalRandom.current.nextInt()
    rmiSpace.register(sessionID, serverSession)
    // tell the client our session is ready
    connection.sendTCP(ServerSessionReady(sessionID))
    // wait for the remote client session to be ready
    val clientSessionReady = received.take().asInstanceOf[ClientSessionReady]
    // create the client session proxy
    session = ObjectSpace.getRemoteObject(connection, clientSessionReady.sessionID, classOf[ClientSession])
    session.asInstanceOf[RemoteObject].setResponseTimeout(TimeOut)
    session = LaggyProxy(session, FakeLag milliseconds, classOf[ClientSession])
    // create the no-reply client session proxy
    sessionNoReply = ObjectSpace.getRemoteObject(connection, clientSessionReady.sessionID, classOf[ClientSession])
    sessionNoReply.asInstanceOf[RemoteObject].setNonBlocking(true)
    sessionNoReply.asInstanceOf[RemoteObject].setResponseTimeout(TimeOut)
    sessionNoReply = LaggyNoReplyProxy(sessionNoReply, FakeLag milliseconds, classOf[ClientSession])
    // integrate the avatar into the world
    server.integrateExternNow(AddEntity(avatar, UUID.randomUUID()))
  }

  def route(events: SortedMap[Long, SortedSet[ChunkEvent]]): Unit = {
    seqExecutor.execute(() => {
      sessionNoReply.integrate(events)
    })
  }

  def setRelation(atTime: Long, subscribed: Set[V3I], updating: Set[V3I], provide: Seq[Chunk]): Unit = {
    seqExecutor.execute(() => {
      sessionNoReply.setServerRelation(atTime, subscribed, updating, provide)
    })
  }

}
