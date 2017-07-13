package com.phoenixkahlo.hellcraft.multiplayertest

import java.net.InetSocketAddress
import java.util.concurrent._

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{GL20, PerspectiveCamera}
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.{Environment, ModelBatch, Renderable, RenderableProvider}
import com.badlogic.gdx.graphics.g3d.utils.FirstPersonCameraController
import com.badlogic.gdx.utils.Pool
import com.esotericsoftware.kryonet.FrameworkMessage.KeepAlive
import com.esotericsoftware.kryonet.Listener.ThreadedListener
import com.esotericsoftware.kryonet.{Client, Connection, KryoSerialization, Listener}
import com.esotericsoftware.kryonet.rmi.{ObjectSpace, RemoteObject}
import com.phoenixkahlo.hellcraft.core.{DefaultTexturePack, ResourceNode, TexturePack}
import com.phoenixkahlo.hellcraft.gamedriver.GameState
import com.phoenixkahlo.hellcraft.math.{Origin, V3F, V3I}
import com.phoenixkahlo.hellcraft.save.GlobalKryo
import com.phoenixkahlo.hellcraft.util.{AsyncExecutor, DependencyGraph, PriorityExecContext}

import scala.collection.JavaConverters

class GameClient(serverAddress: InetSocketAddress) extends Listener with GameState {

  @volatile var ready = false
  val readyMonitor = new Object

  private var received: BlockingQueue[Any] = _
  private var client: Client = _
  private var rmiSpace: ObjectSpace = _
  private var session: ServerSession = _
  private var clientID: ClientID = _

  private var deleted: BlockingQueue[ResourceNode] = _
  private var continuum: ClientContinuum = _
  private var textures: TexturePack = _
  private var cam: PerspectiveCamera = _
  //private var controller: FirstPersonCameraController = _
  private var controller: ClientController = _
  private var modelBatch: ModelBatch = _
  private var lights: Environment = _
  private var vramGraph: DependencyGraph = _
  private var g = 0

  override def onEnter(): Unit = {
    received = new LinkedBlockingDeque

    // connect to the server
    client = new Client(1000000, 1000000, new KryoSerialization(GlobalKryo.create()))
    //GlobalKryo.config(client.getKryo)
    client.addListener(new ThreadedListener(this, AsyncExecutor("client listener thread")))
    client.start()
    client.connect(5000, serverAddress.getAddress, serverAddress.getPort)
    // send the initial data
    client.sendTCP(InitialClientData())
    // receive the client's initial data
    val init = received.take().asInstanceOf[InitialServerData]
    // use the initial data to create a session
    val clientSession = new ClientSessionImpl(init, this)
    rmiSpace = new ObjectSpace
    rmiSpace.setExecutor(AsyncExecutor("client RMI thread"))
    rmiSpace.addConnection(client)
    val clientSessionID = ThreadLocalRandom.current.nextInt()
    rmiSpace.register(clientSessionID, clientSession)
    client.sendTCP(ClientSessionReady(clientSessionID))
    // wait for and setup the remote server session
    val serverSessionReady = received.take().asInstanceOf[ServerSessionReady]
    session = ObjectSpace.getRemoteObject(client, serverSessionReady.sessionID, classOf[ServerSession])
    session.asInstanceOf[RemoteObject].setResponseTimeout(60000)

    // instantiate the other things
    deleted = new LinkedBlockingQueue
    continuum = new ClientContinuum(session,
      session.getStarter() match { case (time, chunkSeq) => (time, chunkSeq.map(chunk => (chunk.pos, chunk)).toMap) })

    textures = new DefaultTexturePack

    cam = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.1f
    cam.far = 1000

    //controller = new FirstPersonCameraController(cam)
    //Gdx.input.setInputProcessor(controller)
    controller = new ClientController(session, cam)

    modelBatch = new ModelBatch

    lights = new Environment
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))

    vramGraph = DependencyGraph()

    readyMonitor.synchronized {
      ready = true
      readyMonitor.notifyAll()
    }
  }

  def waitForReady(): Unit = {
    readyMonitor.synchronized {
      while (!ready)
        readyMonitor.wait()
    }
  }

  override def render(): Unit = {
    println("rendering")
    // prepare
    g += 1
    val world = continuum.current

    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)

    // update the camera controller
    controller.update(continuum.current)

    // get the renderable factories
    val p = V3F(cam.position) / 16 floor
    val chunks = ((p - V3I(3, 3, 3)) to (p + V3I(3, 3, 3))).flatMap(world.weakChunkAt)
    println("rendering " + chunks.size + " chunks")
    val factories = chunks.flatMap(_.renderables(textures, world))
    println("rendering " + factories.size + " factories")

    // manage the resource graph
    val nodes = factories.flatMap(_.resources)
    vramGraph ++= nodes
    if (g % 600 == 0) {
      while (deleted.size > 0) vramGraph --= Seq(deleted.remove())
      PriorityExecContext(Thread.MIN_PRIORITY).execute(() => {
        val garbage = vramGraph.garbage(nodes)
        println("deleting " + garbage.size + " resources")
        Gdx.app.postRunnable(() => garbage.foreach(_.dispose()))
        deleted.addAll(JavaConverters.asJavaCollection(garbage))
      })
    }

    // do the rendering
    val provider = new RenderableProvider {
      override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit =
        factories.flatMap(_()).foreach(renderables.add)
    }
    modelBatch.begin(cam)
    modelBatch.render(provider, lights)
    modelBatch.end()
  }

  override def update(): Boolean = {
    // TODO: predict the server time or something

    var changed = false

    while (continuum.time < session.getTime) {
      continuum.update()
      changed = true
      println("client t = " + continuum.time)
    }

    //changed
    true
  }

  override def onExit(): Unit = {
    client.close()
  }

  override def disconnected(connection: Connection): Unit = {
    Gdx.app.exit()
  }

  override def received(connection: Connection, obj: Any): Unit = {
    if (obj.isInstanceOf[Transmission]) {
      received.add(obj)
    }
  }

  def getContinuum: ClientContinuum = continuum

}
