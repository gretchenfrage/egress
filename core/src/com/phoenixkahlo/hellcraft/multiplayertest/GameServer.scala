package com.phoenixkahlo.hellcraft.multiplayertest

import java.io.File
import java.net.InetAddress
import java.util.UUID
import java.util.concurrent.Executors
import javax.print.DocFlavor.BYTE_ARRAY

import com.esotericsoftware.kryonet.Listener.ThreadedListener
import com.esotericsoftware.kryonet.rmi.ObjectSpace
import com.esotericsoftware.kryonet.{Client, Connection, Listener, Server}
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.save.{GeneratingSave, GlobalKryo, RegionSave, WorldSave}
import com.phoenixkahlo.hellcraft.util.LoopingApp

import scala.collection.parallel
import scala.collection.parallel.{ParMap, mutable}

class GameServer extends Listener with LoopingApp  {

  var save: WorldSave = _
  var continuum: ServerContinuum = _

  var server: Server = _
  var clientIDByConnection: mutable.ParMap[Connection, ClientID] = _
  var clientConnectionByID: mutable.ParMap[ClientID, Connection] = _
  var clientRMISpaces: mutable.ParMap[ClientID, ObjectSpace] = _

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

    server = new Server()
    server.bind(25565)
    GlobalKryo.config(server.getKryo)
    server.addListener(new ThreadedListener(this))

    ObjectSpace.registerClasses(server.getKryo)

    clientIDByConnection = new mutable.ParHashMap
    clientConnectionByID = new mutable.ParHashMap
    clientRMISpaces = new mutable.ParHashMap

    server.start()
  }

  override def update(): Unit = {
    continuum.update()
    continuum.current.pushToSave()
  }

  override def dispose(): Unit = {
    continuum.current.pushToSave()
    save.close()
    server.close()
  }

  override def connected(connection: Connection): Unit = {
    val id = UUID.randomUUID()
    clientIDByConnection.put(connection, id)
    clientConnectionByID.put(id, connection)

    val session = new ServerSessionImpl(id, this)

    val rmiSpace = new ObjectSpace
    rmiSpace.register(1, session)
    rmiSpace.addConnection(connection)
    clientRMISpaces.put(id, rmiSpace)
  }

  override def disconnected(connection: Connection): Unit = {
    val id = clientIDByConnection(connection)
    clientIDByConnection -= connection
    clientConnectionByID -= id
    clientRMISpaces(id).close()
    clientRMISpaces -= id
  }

}