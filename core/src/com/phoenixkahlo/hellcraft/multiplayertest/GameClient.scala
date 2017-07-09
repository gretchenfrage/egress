package com.phoenixkahlo.hellcraft.multiplayertest

import java.net.InetSocketAddress
import java.util.concurrent.{BlockingQueue, LinkedBlockingDeque, LinkedBlockingQueue}
import javax.print.DocFlavor.BYTE_ARRAY

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{GL20, PerspectiveCamera}
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.graphics.g3d.{Environment, ModelBatch, Renderable, RenderableProvider}
import com.badlogic.gdx.graphics.g3d.utils.FirstPersonCameraController
import com.badlogic.gdx.utils.Pool
import com.esotericsoftware.kryonet.Listener.ThreadedListener
import com.esotericsoftware.kryonet.{Client, Connection, KryoSerialization, Listener}
import com.esotericsoftware.kryonet.rmi.ObjectSpace
import com.phoenixkahlo.hellcraft.core.{DefaultTexturePack, ResourceNode, TexturePack}
import com.phoenixkahlo.hellcraft.gamedriver.GameState
import com.phoenixkahlo.hellcraft.math.{Origin, V3F, V3I}
import com.phoenixkahlo.hellcraft.save.GlobalKryo
import com.phoenixkahlo.hellcraft.util.{DependencyGraph, PriorityExecContext}

import scala.collection.JavaConverters

class GameClient(serverAddress: InetSocketAddress) extends Listener with GameState {

  private var received: BlockingQueue[Any] = _
  private var client: Client = _
  private var rmiSpace: ObjectSpace = _
  private var session: ServerSession = _
  private var clientID: ClientID = _

  private var deleted: BlockingQueue[ResourceNode] = _
  private var continuum: ClientContinuum = _
  private var textures: TexturePack = _
  private var cam: PerspectiveCamera = _
  private var controller: FirstPersonCameraController = _
  private var modelBatch: ModelBatch = _
  private var lights: Environment = _
  private var vramGraph: DependencyGraph = _
  private var t = 0
  private var g = 0

  override def onEnter(): Unit = {
    received = new LinkedBlockingDeque

    println("client: connecting to server")
    // connect to the server
    client = new Client(1000000, 1000000, new KryoSerialization(GlobalKryo.create()))
    client.addListener(new ThreadedListener(this))
    client.start()
    client.connect(5000, serverAddress.getAddress, serverAddress.getPort)
    // send the initial data
    client.sendTCP(InitialClientData())
    // receive the client's initial data
    val init = received.take().asInstanceOf[InitialServerData]
    // use the initial data to create a session
    val clientSession = new ClientSessionImpl(init, this)
    rmiSpace = new ObjectSpace
    rmiSpace.addConnection(client)
    rmiSpace.register(1, clientSession)
    client.sendTCP(ClientSessionReady)
    // wait for and setup the remote server session
    val took = received.take()
    if (took != ServerSessionReady)
      throw new ClassCastException
    session = ObjectSpace.getRemoteObject(client, 1, classOf[ServerSession])

    // instantiate the other things
    println("client: instantiating other things")
    deleted = new LinkedBlockingQueue
    continuum = new ClientContinuum(session,
      session.getStarter() match { case (time, chunkSeq) => (time, chunkSeq.map(chunk => (chunk.pos, chunk)).toMap) })

    textures = new DefaultTexturePack

    cam = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    cam.near = 0.1f
    cam.far = 1000

    controller = new FirstPersonCameraController(cam)

    modelBatch = new ModelBatch

    lights = new Environment
    lights.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1))
    lights.add(new DirectionalLight().set(1, 1, 1, 0, -1, 0))

    vramGraph = DependencyGraph()
    println("client: setup")
  }

  override def render(): Unit = {
    println("client rendering")
    // prepare
    g += 1
    val world = continuum.current

    Gdx.gl.glClearColor(0.5089f, 0.6941f, 1f, 1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
    Gdx.gl.glEnable(GL20.GL_TEXTURE_2D)

    // update the camera controller
    controller.update()

    // get the renderable factories
    val p = V3F(cam.position) / 16 floor
    val factories = ((p - V3I(3, 3, 3)) to (p + V3I(3, 3, 3))).map(world.chunkAt(_).get)
      .flatMap(_.renderables(textures, world))

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

  override def update(): Unit = {
    println("client updating")
    t += 1
    continuum.update()
  }

  override def onExit(): Unit = {
    client.close()
  }

  override def received(connection: Connection, obj: Any): Unit = {
    println("client: received " + obj + ", adding to received queue")
    received.add(obj)
  }

  def getContinuum: ClientContinuum = continuum

}
