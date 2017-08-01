package com.phoenixkahlo.hellcraft.multiplayertest

import java.io.{FileOutputStream, PrintStream}
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
import com.esotericsoftware.kryonet.Listener.{LagListener, ThreadedListener}
import com.esotericsoftware.kryonet.{Client, Connection, KryoSerialization, Listener}
import com.esotericsoftware.kryonet.rmi.{ObjectSpace, RemoteObject}
import com.phoenixkahlo.hellcraft.core.entity.Avatar
import com.phoenixkahlo.hellcraft.core.{DefaultTexturePack, ResourceNode, TexturePack, World}
import com.phoenixkahlo.hellcraft.gamedriver.{GameState, RunnableGameState}
import com.phoenixkahlo.hellcraft.math.{Origin, V3F, V3I}
import com.phoenixkahlo.hellcraft.util._
import other.AppDirs
import scala.concurrent.duration._

import scala.collection.JavaConverters

class EgressClient(serverAddress: InetSocketAddress) extends Listener with RunnableGameState {

  @volatile var ready = false
  val readyMonitor = new Object

  private var received: BlockingQueue[Any] = _
  private var kryonetClient: KryonetClient = _
  private var session: ServerSession = _
  private var clientID: ClientID = _
  private var serverNanotime: NanotimeMirror = _
  private var clock: GametimeClock = _

  private var deleted: BlockingQueue[ResourceNode] = _
  private var continuum: ClientContinuum = _
  private var textures: TexturePack = _
  private var cam: PerspectiveCamera = _
  private var controller: ClientController = _
  private var modelBatch: ModelBatch = _
  private var lights: Environment = _
  private var vramGraph: DependencyGraph = _
  private var interpolator: Interpolator = _
  private var g = 0

  override def onEnter(): Unit = {
    received = new LinkedBlockingDeque

    // connect to the server
    kryonetClient = new Client(1000000, 1000000, new KryoSerialization(GlobalKryo.create()))
    kryonetClient.addListener(new LagListener(FakeLag, FakeLag, new ThreadedListener(this, AsyncExecutor("client listener thread"))))
    kryonetClient.start()
    kryonetClient.connect(5000, serverAddress.getAddress, serverAddress.getPort)
    kryonetClient.setTimeout(TimeOut)
    // send the initial data
    kryonetClient.sendTCP(InitialClientData())
    // receive the client's initial data
    val init = received.take().asInstanceOf[InitialServerData]
    // use the initial data to create a session
    val clientSession = new ClientSessionImpl(this)
    val rmiSpace = new ObjectSpace
    rmiSpace.setExecutor(AsyncExecutor("client RMI thread"))
    rmiSpace.addConnection(kryonetClient)
    val clientSessionID = ThreadLocalRandom.current.nextInt()
    rmiSpace.register(clientSessionID, clientSession)
    kryonetClient.sendTCP(ClientSessionReady(clientSessionID))
    // wait for and setup the remote server session
    val serverSessionReady = received.take().asInstanceOf[ServerSessionReady]
    session = ObjectSpace.getRemoteObject(kryonetClient, serverSessionReady.sessionID, classOf[ServerSession])
    session.asInstanceOf[RemoteObject].setResponseTimeout(TimeOut)
    session = LaggyProxy(session, FakeLag milliseconds, classOf[ServerSession])

    // synchronize the times
    serverNanotime = NanotimeMirror.mirrorServer(session)
    clock = GametimeClock.clientClock(session, serverNanotime)

    // instantiate the other things
    deleted = new LinkedBlockingQueue
    continuum = new ClientContinuum(session, clock.gametime)

    textures = new DefaultTexturePack

    cam = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.1f
    cam.far = 1000

    controller = new ClientController(session, cam, this)
    Gdx.input.setInputProcessor(controller)

    modelBatch = new ModelBatch

    lights = new Environment
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))

    vramGraph = DependencyGraph()

    interpolator = new Interpolator(clock, Forward)

    System.out.println("avatarID = [" + controller.avatarID.getMostSignificantBits + ", " + controller.avatarID.getLeastSignificantBits + "]")

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
    // prepare
    g += 1
    val world = continuum.current

    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)

    // interpolate
    val (toRender, interpolation) = interpolator.interpolate(world)

    // update the camera controller
    controller.camUpdate(toRender, interpolation)

    // get the renderable factories
    val p = V3F(cam.position) / 16 floor
    val chunks = ((p - V3I(3, 3, 3)) to (p + V3I(3, 3, 3))).flatMap(toRender.asInstanceOf[ClientWorld].weakChunkAt)
    val factories = chunks.flatMap(_.renderables(textures, world))

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
        factories.flatMap(_(interpolation)).foreach(renderables.add)
    }
    modelBatch.begin(cam)
    modelBatch.render(provider, lights)
    modelBatch.end()
  }

  override def run(): Unit = {
    while (true) {
      val time = continuum.synchronized {
        val submissions = controller.mainUpdate(continuum.current)
        continuum.update(submissions)
        continuum.time
      }
      clock.waitUntil(time + 1)
    }
  }

  override def onExit(): Unit = {
    kryonetClient.close()
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

  def getClock: GametimeClock = clock

  def getKryonetClient: KryonetClient = kryonetClient

}
