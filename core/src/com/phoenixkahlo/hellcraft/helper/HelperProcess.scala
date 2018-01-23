package com.phoenixkahlo.hellcraft.helper

import java.io._
import java.net.{ServerSocket, Socket}
import java.nio.file.{Path, Paths}
import java.util.{Scanner, UUID}

import com.phoenixkahlo.hellcraft.helper.HM._
import com.phoenixkahlo.hellcraft.helper.HelperService.Starter
import com.phoenixkahlo.hellcraft.helper.LocalHelperHalf.{CallLocal, FulfillLocal, RemoteToLocal}
import com.phoenixkahlo.hellcraft.helper.RemoteHelperSide._
import com.phoenixkahlo.hellcraft.util.collections.IdentityKey
import com.phoenixkahlo.hellcraft.util.threading._

import scala.collection.{mutable, parallel}

class RemoteHelperSide(_in: InputStream, _out: OutputStream, _log: OutputStream) {
  implicit def executor = UniExecutor.getService

  val out = new ObjectOutputStream(_out)
  val in = new ObjectInputStream(_in)
  val log = new PrintStream(_log)

  val fulfill = new FulfillmentContext[UUID, Any]
  val sender = new FutSequence(UniExecutor.foreground)
  var procedures: Map[HelperServiceID[_], RemoteHelperServiceProcedure[_]] = Map.empty
  var accessors: Map[HelperServiceID[_], LocalHelperServiceProcedureAccessor[_]] = Map.empty

  private def send(message: RemoteToLocal): Unit = {
    println("enqueueing local send")
    sender(() => {
      println("sending local: " + message)
      out.writeObject(message)
    })
  }

  private def callLocal[S <: HelperService, T](call: S#LocalCall[T], sid: HelperServiceID[S]): Fut[T] = {
    val id = UUID.randomUUID()
    send(CallLocal[S, T](id, sid, call))
    val fut = fulfill.fut(id).asInstanceOf[Fut[T]]
    fut.onComplete(() => fulfill.remove(id))
    fut
  }

  private def compile[T](monad: HM[T])(implicit accum: parallel.mutable.ParMap[IdentityKey[HM[_]], Fut[_]]): Fut[T] =
    accum.get(IdentityKey(monad)).asInstanceOf[Option[Fut[T]]].getOrElse({
      val fut = monad match {
        case HMGen(fac: (() => T), hint) => Fut(fac(), hint.exec)
        case monad: HMMap[_, T] =>
          def f[S](monad: HMMap[S, T]): Fut[T] =
            compile(monad.src).map(monad.func, monad.exec.exec)
          f(monad)
        case monad: HMFMap[_, T] =>
          def f[S](monad: HMFMap[S, T]): Fut[T] =
            compile(monad.src).flatMap(s => compile(monad.func(s)))
          f(monad)
        case HMMisc(fac: (UniExecutor => Fut[T])) => fac(executor)
        case monad: HMSCall[T, _] =>
          def f[S <: HelperService](monad: HMSCall[T, S]): Fut[T] =
            procedures(monad.sid).asInstanceOf[RemoteHelperServiceProcedure[S]].invoke(monad.call, accessors(monad.sid).asInstanceOf[LocalHelperServiceProcedureAccessor[S]])
          f(monad)
      }
      accum.put(IdentityKey(monad), fut)
      fut
    })

  def start(): Unit = {
    // start
    in.readObject() match {
      case Start(services, poolconfig) =>
        // initialize executor
        UniExecutor.activate(new SmartPool(poolconfig))
        // initialize services
        for ((id: HelperServiceID[_], factory: (UniExecutor => RemoteHelperServiceProcedure[_])) <- services) {
          procedures += ((id, factory(UniExecutor.getService)))
          accessors += ((id, new LocalHelperServiceProcedureAccessor[HelperService] {
            override def invoke[T](call: HelperService#LocalCall[T]) = callLocal[HelperService, T](call, id.asInstanceOf[HelperServiceID[HelperService]])
          }))
        }
    }
    // loop
    var shouldContinue = true
    while (shouldContinue) in.readObject().asInstanceOf[LocalToRemote] match {
      // on apply, compile to fut, await completion, and send back result
      case Apply(id, monad) =>
        compile(monad)(new parallel.mutable.ParHashMap)
          .map(result => send(FulfillLocal(id, result)))
      // on call, invoke service, await completion, and send back result
      case call: CallRemote[_, _] =>
        def f[S <: HelperService, T](call: CallRemote[S, T]): Unit = {
          procedures(call.sid).asInstanceOf[RemoteHelperServiceProcedure[S]]
            .invoke(call.call, accessors(call.sid).asInstanceOf[LocalHelperServiceProcedureAccessor[S]])
            .map(t => send(FulfillLocal(call.id, t)))
        }
        f(call)
      // on fulfill, put in fulfillment context
      case FulfillRemote(id, thing) =>
        fulfill.put(id, thing)
    }
  }
}
object RemoteHelperSide {
  sealed trait LocalToRemote extends Serializable
  case class Start(services: Seq[(HelperServiceID[_], UniExecutor => RemoteHelperServiceProcedure[_])], poolconfig: SmartPool.Config) extends LocalToRemote
  case class Apply(id: UUID, monad: HM[_]) extends LocalToRemote
  case class CallRemote[S <: HelperService, T](id: UUID, sid: HelperServiceID[S], call: S#RemoteCall[T]) extends LocalToRemote
  case class FulfillRemote(id: UUID, thing: Any) extends LocalToRemote


  def main(args: Array[String]): Unit = {
    // reroute the System.err into the System.out, to ensure the std err is only used for outwards communication
    //val stdErr = System.err
    System.setErr(System.out)

    println("remote helper started")

    //val port = Integer.parseInt(args(0))
    val port = 39422git //new Scanner(System.in).nextInt()
    println("connecting to port: " + port)
    val socket = new Socket("localhost", port)
    println("connected")
    new RemoteHelperSide(socket.getInputStream, socket.getOutputStream, System.out).start()

    //new RemoteHelperSide(System.in, stdErr, System.out).start()
  }
}

object ProcessHelperLauncher {
  def launch(remConfig: SmartPool.Config): Either[Helper, Exception] = {
    try {
      val jar = Paths.get(getClass().getProtectionDomain.getCodeSource.getLocation.toURI)
      //val builder = new ProcessBuilder("java -Xms5g -Xmx5g -d64 -cp " + jar.toString + " com.phoenixkahlo.hellcraft.helper.RemoteHelperSide")
      //val process = builder.start()
      //val process = Runtime.getRuntime.exec("java -Xms5g -Xmx5g -d64 -cp " + jar.toString + " com.phoenixkahlo.hellcraft.helper.RemoteHelperSide")
      val port = 39422
      println("generated random port: " + port)
      val ssocket = new ServerSocket(port)
      val process = Runtime.getRuntime.exec("java -Xms1g -Xmx6g -d64 -cp C:\\Users\\kahlo\\Desktop\\Intellij\\egress\\desktop\\build\\libs\\desktop-1.0.jar com.phoenixkahlo.hellcraft.helper.RemoteHelperSide")
      new Thread(() => {
        val scanner = new Scanner(process.getInputStream)
        while (true) {
          println(scanner.nextLine())
        }
      }).start()
      new PrintStream(process.getOutputStream).println(port)
      println("wrote port")
      val socket = ssocket.accept()
      if (/*!socket.getInetAddress.isAnyLocalAddress*/false) {
        Right(new Exception("helper socket joined by remote address"))
      } else {
        val out = socket.getOutputStream
        val in = socket.getInputStream
          //val log = process.getInputStream
        Left(new LocalHelperHalf(process, new ObjectOutputStream(out), new ObjectInputStream(in), remConfig)(UniExecutor.getService))
      }

      //Left(new LocalHelperHalf(process, remConfig)(UniExecutor.getService))
    } catch {
      case e: Exception => Right(e)
    }
  }
}

class LocalHelperHalf(process: Process, out: ObjectOutput, in: ObjectInput, /*log: InputStream,*/ remConfig: SmartPool.Config)(implicit executor: UniExecutor) extends Helper {
  //private val process: Process = new ProcessBuilder("java -Xms5g -Xmx5g -d64 -jar ", jar.toString).start()
  //private val out = new ObjectOutputStream(process.getOutputStream)
  //private val in = new ObjectInputStream(process.getErrorStream)
  //private val log = process.getInputStream

  private val fulfill = new FulfillmentContext[UUID, Any]
  private val sender = new FutSequence(UniExecutor.foreground)
  private var procedures: Map[HelperServiceID[_], LocalHelperServiceProcedure[_]] = Map.empty
  private var accessors: Map[HelperServiceID[_], RemoteHelperServiceProcedureAccessor[_]] = Map.empty

  private var threads = new mutable.ArrayBuffer[Thread]
  private def startThread(runnable: Runnable): Unit = {
    val thread = new Thread(runnable)
    threads += thread
    thread.start()
  }

  private def send(message: LocalToRemote): Unit = sender(() => {
    println("sending remote: " + message)
    out.writeObject(message)
  })

  private def callRemote[S <: HelperService, T](call: S#RemoteCall[T], sid: HelperServiceID[S]): Fut[T] = {
    val id = UUID.randomUUID()
    send(CallRemote[S, T](id, sid, call))
    val fut = fulfill.fut(id).asInstanceOf[Fut[T]]
    fut.onComplete(() => fulfill.remove(id))
    fut
  }

  override def apply[T](monad: HM[T]): Fut[T] = {
    val id = UUID.randomUUID()
    send(Apply(id, monad))
    val fut = fulfill.fut(id).asInstanceOf[Fut[T]]
    fut.onComplete(() => fulfill.remove(id))
    fut
  }

  override def start(services: Seq[HelperService.Starter[_ <: HelperService]]): Unit = {
    send(Start(services.map(starter => (starter.id, starter.remote)), remConfig))
    for (Starter(id, local, _) <- services) {
      procedures += ((id, local))
      accessors += ((id, new RemoteHelperServiceProcedureAccessor[HelperService] {
        override def invoke[T](call: HelperService#RemoteCall[T]) = callRemote[HelperService, T](call, id.asInstanceOf[HelperServiceID[HelperService]])
      }))
    }

    startThread(() => {
      try {
        while (!Thread.interrupted()) in.readObject().asInstanceOf[RemoteToLocal] match {
          case call: CallLocal[_, _] =>
            def f[S <: HelperService, T](call: CallLocal[S, T]): Unit = {
              procedures(call.sid).asInstanceOf[LocalHelperServiceProcedure[S]]
                .invoke(call.call, accessors(call.sid).asInstanceOf[RemoteHelperServiceProcedureAccessor[S]])
                .map(t => send(FulfillRemote(call.id, t)))
            }
            f(call)
          case FulfillLocal(id, thing) =>
            fulfill.put(id, thing)
        }
      } catch {
        case ie: InterruptedException => println("helper shutting down")
      }
    })
    /*
    startThread(() => {
      try {
        while (!Thread.interrupted()) {
          System.out.write(log.read())
        }
      } catch {
        case ie: InterruptedException => println("helper shutting down")
      }
    })
    */
  }

  override def close(): Unit = {
    threads.foreach(_.interrupt())
    threads.foreach(_.join())
    process.destroy()
    process.waitFor()
  }
}
object LocalHelperHalf {
  sealed trait RemoteToLocal extends Serializable
  case class CallLocal[S <: HelperService, T](id: UUID, sid: HelperServiceID[S], call: S#LocalCall[T]) extends RemoteToLocal
  case class FulfillLocal(id: UUID, thing: Any) extends RemoteToLocal
}