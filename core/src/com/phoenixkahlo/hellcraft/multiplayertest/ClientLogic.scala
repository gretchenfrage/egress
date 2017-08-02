package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
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
class ClientLogic(server: EgressServer) {

  val clientID: ClientID = UUID.randomUUID()
  val received = new LinkedBlockingQueue[Any]

  def receive(message: Any): Unit = received.add(message)

  @volatile var avatarID: AvatarID = _

  @volatile var session: ClientSession = _
  @volatile var seqSession: ClientSession = _

  @volatile var isInitialized = false

  var lastChunkPos: Option[V3I] = None

  def initialize(connection: Connection): Unit = {
    // configure the connection
    connection.setTimeout(TimeOut)
    // create the RMI space
    val rmiSpace = new ObjectSpace
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
    if (FakeLag > 0)
      session = LaggyProxy(session, FakeLag milliseconds, classOf[ClientSession])
    // create the seq client session proxy
    seqSession = ObjectSpace.getRemoteObject(connection, session.createSingleThreadSession("seq session"),
      classOf[ClientSession])
    seqSession.asInstanceOf[RemoteObject].setNonBlocking(true)
    seqSession.asInstanceOf[RemoteObject].setResponseTimeout(TimeOut)
    if (FakeLag > 0)
      seqSession = LaggyNoReplyProxy(seqSession, FakeLag milliseconds, classOf[ClientSession])
    // integrate the avatar into the world
    server.integrateExternNow(AddEntity(avatar, UUID.randomUUID()))
    // signal that this is intialized
    isInitialized = true
  }

  def route(events: SortedMap[Long, SortedSet[ChunkEvent]]): Unit = {
    seqSession.integrate(events)
  }

  def setRelation(subscribed: Set[V3I], updating: Set[V3I], provide: Seq[Chunk],
                  unpredictable: SortedMap[Long, SortedSet[ChunkEvent]]): Unit = {
    seqSession.setServerRelation(subscribed, updating, provide, unpredictable)
  }

  def update(): Unit = {
    val p = server.continuum.current.findEntity(avatarID).get.asInstanceOf[Avatar].chunkPos
    if (!lastChunkPos.contains(p)) {
      lastChunkPos = Some(p)

      server.setClientRelation(
        clientID,
        (p - SubscribeDistance) to (p + SubscribeDistance) toSet,
        (p - UpdateDistance) to (p + UpdateDistance) toSet
      )
    }
  }

}
