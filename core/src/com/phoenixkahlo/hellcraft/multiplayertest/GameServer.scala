package com.phoenixkahlo.hellcraft.multiplayertest

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}
import java.net.InetAddress
import java.util.UUID
import java.util.concurrent._
import javax.print.DocFlavor.BYTE_ARRAY

import com.esotericsoftware.kryonet.FrameworkMessage.KeepAlive
import com.esotericsoftware.kryonet.Listener.ThreadedListener
import com.esotericsoftware.kryonet.rmi.{ObjectSpace, RemoteObject}
import com.esotericsoftware.kryonet._
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Avatar
import com.phoenixkahlo.hellcraft.gamedriver.LoopingApp
import com.phoenixkahlo.hellcraft.math.{Origin, V3F, V3I}
import com.phoenixkahlo.hellcraft.save.{GeneratingSave, RegionSave, WorldSave}
import com.phoenixkahlo.hellcraft.util.{AsyncExecutor, GlobalKryo}
import com.twitter.chill.{Input, Output}

import scala.collection.immutable.{HashSet, TreeMap, TreeSet}
import scala.collection.mutable.ArrayBuffer
import scala.collection.{SortedMap, SortedSet, parallel}
import scala.collection.parallel
import scala.collection.mutable
import scala.concurrent.ExecutionContext

class GameServer(port: Int) extends Listener with LoopingApp {

  var save: WorldSave = _
  var continuum: ServerContinuum = _

  var server: Server = _
  var clientIDByConnection: parallel.mutable.ParMap[Connection, ClientID] = _
  var clientConnectionByID: parallel.mutable.ParMap[ClientID, Connection] = _
  var clientRMISpaces: parallel.mutable.ParMap[ClientID, ObjectSpace] = _
  var clientSessions: parallel.mutable.ParMap[ClientID, ClientSession] = _
  var clientSeqExecutors: parallel.mutable.ParMap[ClientID, ExecutionContext] = _
  var received: parallel.mutable.ParMap[ClientID, BlockingQueue[Any]] = _
  var avatars: parallel.mutable.ParMap[ClientID, AvatarID] = _

  override def init(deactivator: Runnable): Unit = {
    val saveFolder = new File("C:\\Users\\Phoenix\\Desktop\\mul")
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
    server.addListener(new ThreadedListener(this, AsyncExecutor("server listener thread")))

    clientIDByConnection = new parallel.mutable.ParHashMap
    clientConnectionByID = new parallel.mutable.ParHashMap
    clientRMISpaces = new parallel.mutable.ParHashMap
    clientSessions = new parallel.mutable.ParHashMap
    clientSeqExecutors = new parallel.mutable.ParHashMap
    avatars = new parallel.mutable.ParHashMap

    // received will automatically create queues when one is requested for a new client ID
    received = new parallel.mutable.ParHashMap[ClientID, BlockingQueue[Any]] {
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

    val times = new ArrayBuffer[Long]
    val log: () => Unit = () => times += System.nanoTime()

    log()

    val (time, toRoute) = continuum.synchronized {
      (continuum.time, continuum.update())
    }

    println("SERVER t = " + time)

    log()

    for ((client, events) <- toRoute) {
      clientSeqExecutors(client).execute(() => {
        clientSessions(client).integrate(new TreeMap[Long, SortedSet[ChunkEvent]]().updated(time, events))
      })
    }

    log()

    continuum.current.pushToSave()

    log()

    /*
    print("server update times = [")
    for (i <- 0 until times.size - 1)
      print((times(i + 1) - times(i)) / 1000000 + "ms, ")
    println("]")
    */
  }

  def setClientRelation(client: ClientID, subscribed: Set[V3I], updatingSet: Set[V3I]): Unit = {
    val (time, provide) = continuum.synchronized {
      (continuum.time, continuum.setClientRelation(client, subscribed, updatingSet))
    }
    clientSeqExecutors(client).execute(() => {
      clientSessions(client).setServerRelation(time, subscribed, updatingSet, provide)
    })
  }

  def integrateExterns(newExterns: SortedMap[Long, Set[ChunkEvent]]): Unit = {
    for ((client, events: SortedMap[Long, SortedSet[ChunkEvent]]) <- continuum.integrateExterns(newExterns)) {
      clientSeqExecutors(client).execute(() => {
        clientSessions(client).integrate(events)
      })
    }
  }

  def integrateExterns(atTime: Long, newExterns: Set[ChunkEvent]): Unit =
    integrateExterns(new TreeMap[Long, Set[ChunkEvent]]().updated(atTime, newExterns))

  def integrateExtern(atTime: Long, extern: ChunkEvent): Unit =
    integrateExterns(atTime, new HashSet[ChunkEvent] + extern)

  def integrateExternsNow(newExterns: Set[ChunkEvent]): Unit =
    integrateExterns(continuum.time, newExterns)

  def integrateExternNow(extern: ChunkEvent): Unit =
    integrateExternsNow(new HashSet[ChunkEvent] + extern)

  override def dispose(): Unit = {
    continuum.current.pushToSave()
    save.close()
    server.close()
  }

  override def connected(connection: Connection): Unit = {
    // listen
    connection.addListener(new ThreadedListener(this, AsyncExecutor("server client listener thread")))
    // give the client an ID
    val clientID = UUID.randomUUID()
    clientIDByConnection.put(connection, clientID)
    clientConnectionByID.put(clientID, connection)
    clientSeqExecutors.put(clientID, ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(runnable => new Thread(runnable, "client seq thread"))))

    // send the initial data
    connection.sendTCP(InitialServerData(clientID))
    // receive the client's initial data
    val init = received(clientID).take().asInstanceOf[InitialClientData]
    // use the initial data to create a session
    val serverSession = new ServerSessionImpl(init, this, clientID)
    val rmiSpace = new ObjectSpace
    rmiSpace.setExecutor(AsyncExecutor("server RMI thread"))
    clientRMISpaces.put(clientID, rmiSpace)
    rmiSpace.addConnection(connection)
    val sessionID = ThreadLocalRandom.current().nextInt()
    rmiSpace.register(sessionID, serverSession)
    // tell the client the session is ready
    connection.sendTCP(ServerSessionReady(sessionID))
    // wait for and setup the remote client session
    val clientSessionReady = received(clientID).take().asInstanceOf[ClientSessionReady]
    val clientSession = ObjectSpace.getRemoteObject(connection, clientSessionReady.sessionID, classOf[ClientSession])
    clientSession.asInstanceOf[RemoteObject].setResponseTimeout(60000)
    clientSessions.put(clientID, clientSession)

    // for now, make it so that each client just subscribes to the same chunk of chunks
    setClientRelation(
      clientID,
      (Origin - V3I(5, 5, 5)) to (Origin + V3I(5, 5, 5)) toSet,
      (Origin - V3I(3, 3, 3)) to (Origin + V3I(3, 3, 3)) toSet
    )

    // create the avatar
    val avatar = Avatar(pos = V3F(0, 10, 0))
    integrateExternNow(PutEntity(avatar.chunkPos, avatar, UUID.randomUUID()))
    avatars.synchronized {
      avatars.put(clientID, avatar.id)
      avatars.notifyAll()
    }

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
    clientSeqExecutors -= id
    received -= id
  }

}
