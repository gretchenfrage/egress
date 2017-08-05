package com.phoenixkahlo.hellcraft.multiplayertest

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}
import java.net.InetAddress
import java.util.UUID
import java.util.concurrent._
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import javax.print.DocFlavor.BYTE_ARRAY

import com.esotericsoftware.kryonet.FrameworkMessage.KeepAlive
import com.esotericsoftware.kryonet.Listener.{LagListener, ThreadedListener}
import com.esotericsoftware.kryonet.rmi.{ObjectSpace, RemoteObject}
import com.esotericsoftware.kryonet._
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Avatar
import com.phoenixkahlo.hellcraft.gamedriver.LoopingApp
import com.phoenixkahlo.hellcraft.math.{Origin, V3F, V3I}
import com.phoenixkahlo.hellcraft.save.{GeneratingRegionSave, GeneratingSave, RegionSave, WorldSave}
import com.phoenixkahlo.hellcraft.util._
import com.twitter.chill.{Input, Output}
import other.AppDirs

import scala.collection.immutable.{HashSet, TreeMap, TreeSet}
import scala.collection.mutable.ArrayBuffer
import scala.collection.{SortedMap, SortedSet, parallel}
import scala.collection.parallel
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class EgressServer extends Listener with Runnable {

  var save: WorldSave = _
  var continuum: ServerContinuum = _
  var clock: GametimeClock = _

  var kryonetServer: KryonetServer = _

  var clientIDByConnection: parallel.mutable.ParMap[Connection, ClientID] = _
  var clientConnectionByID: parallel.mutable.ParMap[ClientID, Connection] = _

  var clientLogics: parallel.mutable.ParMap[ClientID, ClientLogic] = _

  val savingFlag = new AtomicBoolean(false)

  def this(port: Int) = {
    this()

    val saveFolder = AppDirs.dataDir("egress").resolve("mul").toFile
    saveFolder.mkdir()
    save = new GeneratingRegionSave(saveFolder toPath, 24, v => {
      if (v.y < -20) Stone
      else if (v.y < 0) Dirt
      else if (v.y == 0) Grass
      else Air
    })

    clock = GametimeClock.serverClock()

    continuum = new ServerContinuum(save)

    kryonetServer = new KryonetServer(BufferSize, BufferSize, new KryoSerialization(GlobalKryo.create()))
    kryonetServer.bind(port)
    kryonetServer.addListener(new LagListener(FakeLag, FakeLag, new ThreadedListener(this,
      Executors.newSingleThreadExecutor(runnable => new Thread(runnable, "server listener thread")))))

    clientIDByConnection = new parallel.mutable.ParHashMap
    clientConnectionByID = new parallel.mutable.ParHashMap
    clientLogics = new parallel.mutable.ParHashMap

    kryonetServer.start()
  }

  override def run(): Unit = {
    while (true) {
      // update and route events to clients as needed
      val (time, toRoute) = continuum.synchronized {
        (continuum.time, continuum.update())
      }
      //println("T = " + time)
      for ((client, events) <- toRoute) {
        clientLogics(client).route(new TreeMap[Long, SortedSet[ChunkEvent]]().updated(time, events))
      }
      // push the current world to the save, if the time is right
      val current = continuum.current
      if (time % 600 == 0 && !savingFlag.getAndSet(true)) PriorityExecContext(1).execute(() => {
        println("saving")
        current.pushToSave()
        println("finished saving")
        savingFlag.set(false)
      })
      // update the client logics
      clientLogics.values.filter(_ isInitialized).foreach(_.update())
      // possibly sleep until the next tick
      clock.waitUntil(time + 1)
    }
  }

  def setClientRelation(client: ClientID, subscribed: Set[V3I], updating: Set[V3I]): Unit = {
    continuum.synchronized {
      val logic = clientLogics(client)
      val (provide, unpredictable) = continuum.setClientRelation(client, continuum.time - 50, subscribed, updating)
      logic.setRelation(subscribed, updating, provide, unpredictable)
    }
  }

  def integrateExterns(newExterns: SortedMap[Long, Set[ChunkEvent]]): Unit = {
    continuum.synchronized {
      for ((client, events) <- continuum.integrateExterns(newExterns)) {
        clientLogics(client).route(events)
      }
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

  override def connected(connection: Connection): Unit = {
    val logic = new ClientLogic(this)
    clientLogics.put(logic.clientID, logic)
    clientIDByConnection.put(connection, logic.clientID)
    clientConnectionByID.put(logic.clientID, connection)
    AsyncExecutor run {
      // initialize the connection
      logic.initialize(connection)
    }
  }

  override def received(connection: Connection, obj: Any): Unit = {
    if (obj.isInstanceOf[Transmission]) {
      clientLogics(clientIDByConnection(connection)).receive(obj)
    }
  }

  override def disconnected(connection: Connection): Unit = {
    val id = clientIDByConnection(connection)
    clientIDByConnection -= connection
    clientConnectionByID -= id
    clientLogics -= id
  }

}
