package com.phoenixkahlo.hellcraft.multiplayertest

import java.io.File
import java.net.InetAddress
import java.util.UUID
import java.util.concurrent._
import javax.print.DocFlavor.BYTE_ARRAY

import com.esotericsoftware.kryonet.FrameworkMessage.KeepAlive
import com.esotericsoftware.kryonet.Listener.ThreadedListener
import com.esotericsoftware.kryonet.rmi.{ObjectSpace, RemoteObject}
import com.esotericsoftware.kryonet._
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.gamedriver.LoopingApp
import com.phoenixkahlo.hellcraft.math.{Origin, V3I}
import com.phoenixkahlo.hellcraft.save.{GeneratingSave, GlobalKryo, RegionSave, WorldSave}

import scala.collection.immutable.TreeMap
import scala.collection.{SortedMap, SortedSet, parallel}
import scala.collection.parallel.{ParMap, mutable}

class GameServer(port: Int) extends Listener with LoopingApp  {

  var save: WorldSave = _
  var continuum: ServerContinuum = _

  var server: Server = _
  var clientIDByConnection: mutable.ParMap[Connection, ClientID] = _
  var clientConnectionByID: mutable.ParMap[ClientID, Connection] = _
  var clientRMISpaces: mutable.ParMap[ClientID, ObjectSpace] = _
  var clientSessions: mutable.ParMap[ClientID, ClientSession] = _
  var received: mutable.ParMap[ClientID, BlockingQueue[Any]] = _

  override def init(deactivator: Runnable): Unit = {
    val saveFolder = new File("C:\\Users\\kahlo\\Desktop\\mul")
    saveFolder.mkdir()
    save = new GeneratingSave(new RegionSave(saveFolder toPath, 24), v => {
      if (v.y < -20) Stone
      else if (v.y < 0) Dirt
      else if (v.y == 0) Grass
      else Air
    })

    continuum = new ServerContinuum(save)

    server = new Server(1000000, 1000000, new KryoSerialization(GlobalKryo.create()))
    server.bind(port)
    GlobalKryo.config(server.getKryo)
    server.addListener(new ThreadedListener(this, NetworkExecutor()))

    ObjectSpace.registerClasses(server.getKryo)

    clientIDByConnection = new mutable.ParHashMap
    clientConnectionByID = new mutable.ParHashMap
    clientRMISpaces = new mutable.ParHashMap
    clientSessions = new mutable.ParHashMap

    // received will automatically create queues when one is requested for a new client ID
    received = new mutable.ParHashMap[ClientID, BlockingQueue[Any]] {
      override def get(key: ClientID): Option[BlockingQueue[Any]] = {
        super.get(key) match {
          case queue if queue isDefined => queue
          case None =>
            val queue = new LinkedBlockingQueue[Any]
            put(key, queue)
            Some(queue)
        }
      }
    }

    server.start()
  }

  override def update(): Unit = {
    // update and route events to clients as needed
    val (time, toRoute) = continuum.synchronized { (continuum.time, continuum.update()) }
    for ((client, events) <- toRoute) {
      clientSessions(client).integrate(new TreeMap[Long, SortedSet[ChunkEvent]]().updated(time, events))
    }
    continuum.current.pushToSave()
  }

  def setClientRelation(client: ClientID, subscribed: Set[V3I], updatingSet: Set[V3I]): Unit = {
    val (time, provide) = continuum.synchronized {
      (continuum.time, continuum.setClientRelation(client, subscribed, updatingSet))
    }
    clientSessions(client).setServerRelation(time, subscribed, updatingSet, provide)
  }

  def integrateExterns(newExterns: SortedMap[Long, Set[ChunkEvent]]): Unit = {
    for ((client, events) <- continuum.integrateExterns(newExterns))
      clientSessions(client).integrate(events)
  }

  override def dispose(): Unit = {
    continuum.current.pushToSave()
    save.close()
    server.close()
  }

  override def connected(connection: Connection): Unit = {
    // listen
    connection.addListener(new ThreadedListener(this))

    // give the client an ID
    val id = UUID.randomUUID()
    clientIDByConnection.put(connection, id)
    clientConnectionByID.put(id, connection)

    // send the initial data
    connection.sendTCP(InitialServerData(id))
    // receive the client's initial data
    val init = received(id).take().asInstanceOf[InitialClientData]
    // use the initial data to create a session
    val serverSession = new ServerSessionImpl(init, this, id)
    val rmiSpace = new ObjectSpace
    rmiSpace.setExecutor(NetworkExecutor())
    clientRMISpaces.put(id, rmiSpace)
    rmiSpace.addConnection(connection)
    val sessionID = ThreadLocalRandom.current().nextInt()
    rmiSpace.register(sessionID, serverSession)
    connection.sendTCP(ServerSessionReady(sessionID))
    // wait for and setup the remote client session
    val clientSessionReady = received(id).take().asInstanceOf[ClientSessionReady]
    val clientSession = ObjectSpace.getRemoteObject(connection, clientSessionReady.sessionID, classOf[ClientSession])
    clientSession.asInstanceOf[RemoteObject].setResponseTimeout(60000)
    clientSessions.put(id, clientSession)

    // for now, make it so that each client just subscribes to the same chunk of chunks
    setClientRelation(
      id,
      (Origin - V3I(5, 5, 5)) to (Origin + V3I(5, 5, 5)) toSet,
      (Origin - V3I(3, 3, 3)) to (Origin + V3I(3, 3, 3)) toSet
    )
  }

  override def received(connection: Connection, obj: Any): Unit = {
    if (obj.isInstanceOf[Transmission]) {
      received(clientIDByConnection(connection)).add(obj)
    }
  }

  override def disconnected(connection: Connection): Unit = {
    val id = clientIDByConnection(connection)
    clientIDByConnection -= connection
    clientConnectionByID -= id
    clientRMISpaces(id).close()
    clientRMISpaces -= id
    received -= id
  }

}
