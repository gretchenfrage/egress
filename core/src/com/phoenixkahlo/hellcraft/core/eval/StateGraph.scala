package com.phoenixkahlo.hellcraft.core.eval

import java.util.UUID
import java.util.concurrent.ThreadFactory
import java.util.function.Consumer

import com.phoenixkahlo.hellcraft.core.eval.Eval._
import com.phoenixkahlo.hellcraft.util.collections.spatial.SpatialTemporalQueue
import com.phoenixkahlo.hellcraft.util.collections.{Identity, IdentityKey, IdentityMap, TypeMatchingMap}
import com.phoenixkahlo.hellcraft.util.threading.{Fut, Never, UniExecutor}

import scala.collection.mutable.ArrayBuffer

class AsyncEval[T, C <: Eval.Context](root: Eval[T, C]) {
  private type Node[T] = Eval[T, C]
  private type InputMap = TypeMatchingMap[C#InKey, Identity, Any]
  private type NID = IdentityKey[Node[Any]]
  private def iden(node: Node[_]): NID = IdentityKey(node)

  private var applyID: UUID = UUID.randomUUID()
  private var known: Map[NID, Fut[Any]] = Map.empty
  private var unknown: Map[NID, Fut[Any]] = Map.empty
  private var triggers: Map[NID, Seq[NID]] = Map.empty
  private var itriggers: Map[C#InKey[Any], Seq[NID]] = Map.empty
  private var lastin: Option[InputMap] = None

  private def reify[T](node: Node[T])(implicit inputs: InputMap, pack: C#EvalAsync): Fut[T] = synchronized {
    node match {
      case ncreate@ECreate(fac, hint) =>
        // create will always be known and will be triggered by nothing
        // so it's like a memo func but with simpler locking mechanism
        known.get(IdentityKey(ncreate)) match {
          case Some(fut) => fut.asInstanceOf[Fut[T]]
          case None =>
            val fut = Fut(fac(), hint.exec(_)(pack.service))
            known += (IdentityKey(ncreate) -> fut)
            fut
        }
      case ninput@EInput(key: C#InKey[T]) =>
        // input will also always be known and will be triggered by input changes
        known.get(IdentityKey(ninput)) match {
          case Some(fut) => fut.asInstanceOf[Fut[T]]
          case None =>
            val fut = inputs.get(key).map(t => Fut(t, _.run())).getOrElse(Never)
            known = known + (iden(ninput) -> fut)
            itriggers += (key -> (itriggers.getOrElse(key, Seq.empty) :+ iden(ninput)))
            fut
        }
      case nmap: EMap[_, T, C] =>
        // map will always be known and will be triggered by its source
        known.get(IdentityKey(nmap)) match {
          case Some(fut) => fut.asInstanceOf[Fut[T]]
          case None =>
            // some helping the type system
            def f[S](nmap: EMap[S, T, C]): Fut[T] = {
              val fut = reify(nmap.src).map(nmap.func, nmap.hint.exec(_)(pack.service))
              known += (IdentityKey(nmap) -> fut)
              triggers += (IdentityKey(nmap.src) -> (triggers.getOrElse(IdentityKey(nmap.src), Seq.empty) :+ iden(nmap)))
              fut
            }
            f(nmap)
        }
      case nfmap: EFMap[_, T, C] =>
        // flat map will not be immediately known and will be triggered by its source and by the function result
        known.get(IdentityKey(nfmap)) match {
            // first, look in the known section
          case Some(fut) => fut.asInstanceOf[Fut[T]]
          case None =>
            unknown.get(IdentityKey(nfmap)) match {
                // then, look in the unknown section
              case Some(fut) => fut.asInstanceOf[Fut[T]]
                // then create
              case None =>
                // we help the type system
                def f[S](nfmap: EFMap[S, T, C]): Fut[T] = {
                  val fut0: Fut[Node[T]] = reify(nfmap.src).map(nfmap.func)
                  val fut1: Fut[T] = fut0.flatMap(reify(_))
                  unknown += (IdentityKey(nfmap) -> fut1)
                  // on initial mapping completion, transfer to known section, if ID is unchanged
                  val beforeID = applyID
                  fut0.onComplete(() => AsyncEval.this.synchronized {
                    if (AsyncEval.this.applyID == beforeID) {
                      val n: Node[T] = fut0.query.get
                      unknown -= IdentityKey(nfmap)
                      known += (IdentityKey(nfmap) -> fut1)
                      triggers += (IdentityKey(nfmap.src) -> (triggers.getOrElse(IdentityKey(nfmap.src), Seq.empty) :+ iden(nfmap)))
                      triggers += (IdentityKey(n) -> (triggers.getOrElse(IdentityKey(n), Seq.empty) :+ iden(nfmap)))
                    }
                  })
                  fut1
                }
                f(nfmap)
            }
        }
      case nfilter@EFilter(src, test, hint) =>
        // filter will always be known and will be triggered by its source
        known.get(iden(nfilter)) match {
          case Some(fut) => fut.asInstanceOf[Fut[T]]
          case None =>
            val fut = reify(src).filter(test, hint.exec(_)(pack.service))
            known += (iden(nfilter) -> fut)
            triggers += (iden(src) -> (triggers.getOrElse(iden(src), Seq.empty) :+ iden(nfilter)))
            fut
        }
      case nextern@EExtern(sync, async, trigs) =>
        known.get(iden(nextern)) match {
          case Some(fut) => fut.asInstanceOf[Fut[T]]
          case None =>
            val fut = async(pack)
            known += (iden(nextern) -> fut)
            for (t <- trigs)
              itriggers += (t -> (itriggers.getOrElse(t, Seq.empty) :+ iden(nextern)))
            fut
        }
      case nsmap: ESpecialMap[_, T, C] =>
        // special map will always be known and will be triggered by its source
        known.get(iden(nsmap)) match {
          case Some(fut) => fut.asInstanceOf[Fut[T]]
          case None =>
            def f[S](nsmap: ESpecialMap[S, T, C]): Fut[T] = {
              val fut = reify(nsmap.src).map(nsmap.func, nsmap.exec(pack))
              known += (iden(nsmap) -> fut)
              triggers += (iden(nsmap.src) -> (triggers.getOrElse(iden(nsmap.src), Seq.empty) :+ iden(nsmap)))
              fut
            }
            f(nsmap)
        }
    }
  }

  def fut(in: TypeMatchingMap[C#InKey, Identity, Any], pack: C#EvalAsync): Fut[T] = this.synchronized {
    // step 1: create new ID to kill phantom changes from last application
    applyID = UUID.randomUUID()

    // step 2: invalidate
    if (lastin.nonEmpty && (in ne lastin.get) && in != lastin.get) {
      // clear the unknown, they cannot be trusted
      unknown = Map.empty

      // find the changed inputs
      val changed: Seq[C#InKey[Any]] =
        (lastin.get.keySet ++ in.keySet).toSeq.filter(k => lastin.get.unsafeget(k) != in.unsafeget(k)).asInstanceOf[Seq[C#InKey[Any]]]



      // traverse the known graph, starting at the changed inputs
      val triggered: Set[IdentityKey[Node[_]]] = {
        type N = IdentityKey[Node[_]]

        def traverse(start: N, found: Set[N]): Set[N] =
          triggers.get(start) match {
            case Some(seq) => seq.filterNot(found).foldLeft(found)({ case (a, n) => a + n ++ traverse(n, a + n) })
            case None => Set.empty
          }

        val roots: Seq[N] = changed.flatMap(itriggers.getOrElse(_, Seq.empty))

        roots.foldLeft(roots.to[Set])({ case (a, n) => a ++ traverse(n, a) })
      }

      // clear the triggered nodes
      known --= triggered
    }

    // step 3: set the new last input
    lastin = Some(in)

    // step 4: reify the root, recursively reifying the whole monad graph as needed
    reify(root)(in, pack)
  }

  def query(in: TypeMatchingMap[C#InKey, Identity, Any], pack: C#EvalAsync): Option[T] = fut(in, pack).query

  def query(in: TypeMatchingMap[C#InKey, Identity, Any])(implicit unitIsPack: Unit => C#EvalAsync): Option[T] =
    query(in, unitIsPack(()))
}

class SyncEval[T, C <: Eval.Context](root: Eval[T, C]) {
  private type Node[T] = Eval[T, C]
  private type InputMap = TypeMatchingMap[C#InKey, Identity, Any]
  private type NID = IdentityKey[Node[Any]]
  private implicit def iden(node: Node[_]): NID = IdentityKey(node)

  private var known: Map[NID, Option[Any]] = Map.empty
  private var triggers: Map[NID, Seq[NID]] = Map.empty
  private var itriggers: Map[C#InKey[Any], Seq[NID]] = Map.empty
  private var lastin: Option[InputMap] = None

  private def reify[T](node: Node[T])(implicit inputs: InputMap, pack: C#EvalSync): Option[T] =
    node match {
      case ncreate@ECreate(fac, hint) =>
        known.get(ncreate) match {
          case Some(opt) => opt.asInstanceOf[Option[T]]
          case None =>
            val opt = Some(fac())
            known += (iden(ncreate) -> opt)
            opt
        }
      case ninput@EInput(key: C#InKey[T]) =>
        known.get(ninput) match {
          case Some(opt) => opt.asInstanceOf[Option[T]]
          case None =>
            val opt = inputs.get(key)
            known += (iden(ninput) -> opt)
            itriggers += (key -> (itriggers.getOrElse(key, Seq.empty) :+ iden(ninput)))
            opt
        }
      case nmap: EMap[_, T, C] =>
        known.get(nmap) match {
          case Some(opt) => opt.asInstanceOf[Option[T]]
          case None =>
            def f[S](nmap: EMap[S, T, C]): Option[T] = {
              val opt = reify(nmap.src).map(nmap.func)
              known += (iden(nmap) -> opt)
              triggers += (iden(nmap.src) -> (triggers.getOrElse(nmap.src, Seq.empty) :+ iden(nmap)))
              opt
            }
            f(nmap)
        }
      case nfmap: EFMap[_, T, C] =>
        known.get(nfmap) match {
          case Some(opt) => opt.asInstanceOf[Option[T]]
          case None =>
            def f[S](nfmap: EFMap[S, T, C]): Option[T] = {
              val opt0: Option[Node[T]] = reify(nfmap.src).map(nfmap.func)
              val opt1: Option[T] = opt0.flatMap(n => reify(n))
              known += (iden(nfmap) -> opt1)
              triggers += (iden(nfmap.src) -> (triggers.getOrElse(nfmap.src, Seq.empty) :+ iden(nfmap)))
              for (n <- opt0) {
                triggers += (iden(n) -> (triggers.getOrElse(n, Seq.empty) :+ iden(nfmap)))
              }
              opt1
            }
            f(nfmap)
        }
      case nfilter@EFilter(src, test, hint) =>
        known.get(nfilter) match {
          case Some(opt) => opt.asInstanceOf[Option[T]]
          case None =>
            val opt = reify(src).filter(test)
            known += (iden(nfilter) -> opt)
            triggers += (iden(src) -> (triggers.getOrElse(src, Seq.empty) :+ iden(nfilter)))
            opt
        }
      case nextern@EExtern(sync, async, trigs) =>
        known.get(iden(nextern)) match {
          case Some(opt) => opt.asInstanceOf[Option[T]]
          case None =>
            val opt = sync(pack)
            known += (iden(nextern) -> opt)
            for (t <- trigs)
              itriggers += (t -> (itriggers.getOrElse(t, Seq.empty) :+ iden(nextern)))
            opt
        }
      case nsmap@ESpecialMap(src, func, exec) =>
        known.get(nsmap) match {
          case Some(opt) => opt.asInstanceOf[Option[T]]
          case None =>
            def f[S](nsmap: ESpecialMap[S, T, C]): Option[T] = {
              val opt = reify(nsmap.src).map(nsmap.func)
              known += (iden(nsmap) -> opt)
              triggers += (iden(nsmap.src) -> (triggers.getOrElse(nsmap.src, Seq.empty) :+ iden(nsmap)))
              opt
            }
            f(nsmap)
        }
    }

  def query(in: TypeMatchingMap[C#InKey, Identity, Any], pack: C#EvalSync): Option[T] = {
    if (lastin.nonEmpty && (in ne lastin.get) && in != lastin.get) {
      // find the changed inputs
      var changed = new ArrayBuffer[C#InKey[Any]]
      for ((k: C#InKey[Any], v: Any) <- in.toSeq) {
        if (lastin.get.get(k).get != v)
          changed += k
      }

      // traverse the known graph, starting at the changed inputs
      val triggered: Set[IdentityKey[Node[_]]] = {
        type N = IdentityKey[Node[_]]

        def traverse(start: N, found: Set[N]): Set[N] =
          triggers.get(start) match {
            case Some(seq) => seq.filterNot(found).foldLeft(found)({ case (a, n) => a + n ++ traverse(n, a + n) })
            case None => Set.empty
          }

        val roots: Seq[N] = changed.flatMap(itriggers.getOrElse(_, Seq.empty))

        roots.foldLeft(roots.to[Set])({ case (a, n) => a ++ traverse(n, a) })
      }

      // clear the triggered nodes
      known --= triggered
    }

    lastin = Some(in)

    reify(root)(in, pack)
  }

  def query(in: TypeMatchingMap[C#InKey, Identity, Any])(implicit unitIsPack: Unit => C#EvalSync): Option[T] =
    query(in, unitIsPack(()))
}
/*
object EvalTest extends App {
  import scala.concurrent.duration._
  UniExecutor.activate(
    1,
    new Thread(_),
    _.printStackTrace(),
    SpatialTemporalQueue.timeDoesntMatter
  )

  sealed trait IK[+T]
  case object K1 extends IK[String]
  case object K2 extends IK[String]
  case object K3 extends IK[Int]
  case object K4 extends IK[Int]



  trait TestContext extends Eval.Context {
    override type InKey[+T] = IK[T]
    override type EvalAsync = Eval.UniExecProvider
    override type EvalSync = Unit
  }

  val i1 = NInput[String, TestContext](K1)
  val i2 = NInput[String, TestContext](K2)
  val i3 = NInput[Int, TestContext](K3)
  val i4 = NInput[Int, TestContext](K4)

  implicit val hint = ExecSeq
  val r = for {
    str1 <- i1
    str2 <- i2
    n <- i3
  } yield str1 + Stream.iterate("-")(identity).take(n).fold("")(_ + _) + str2

  val graph = new SyncEval(r)

  var in = TypeMatchingMap[IK, Identity, Any](
    K1 -> "hello",
    K2 -> "world",
    K3 -> 5,
    K4 -> 7
  )
  val a = graph.query(in)
  println(a)

  in += (K3 -> 3)
  val b = graph.query(in)
  println(b)

  in += (K2 -> "hell")
  val c = graph.query(in)

  in += (K1 -> "goodbye")
  val d = graph.query(in)

  println(d)
  println(c)

  in = TypeMatchingMap[IK, Identity, Any](
    K1 -> "phoenix",
    K2 -> "kahlo",
    K4 -> 1
  )
  println(graph.query(in))

  /*
  val graph = new AsyncEval(r)

  val provider = new Eval.UniExecProvider {
    override def service: UniExecutor = UniExecutor.getService
  }

  var in = TypeMatchingMap[IK, Identity, Any](
    K1 -> "hello",
    K2 -> "world",
    K3 -> 5,
    K4 -> 7
  )
  val af = graph.fut(in, provider)
  val a = af.await
  println(a)

  in += (K3 -> 3)
  val bf = graph.fut(in, provider)
  val b = bf.await
  println(b)

  in += (K2 -> "hell")
  val cf = graph.fut(in, provider)

  in += (K1 -> "goodbye")
  val df = graph.fut(in, provider)

  println(df.await)
  println(cf.await)

  in = TypeMatchingMap[IK, Identity, Any](
    K1 -> "phoenix",
    K2 -> "kahlo",
    K3 -> 10,
    K4 -> 1
  )
  println(graph.fut(in, provider).await)
  */

  UniExecutor.deactivate()
}
*/
/*
object Eval {
  trait UniExecProvider {
    def service: UniExecutor
  }
  trait Context {
    type InKey[+T]
    type EvalAsync <: UniExecProvider
    type EvalSync
  }

  sealed trait Node[+T, C <: Context] extends Serializable {
    def map[N](func: T => N)(implicit hint: ExecHint): Node[N, C] = NMap(this, func, hint)
    def flatMap[N](func: T => Node[N, C])(implicit hint: ExecHint): Node[N, C] = NFMap(this, func, hint)
    def filter(test: T => Boolean)(implicit hint: ExecHint): Node[T, C] = NFilter(this, test, hint)
    def withFilter(test: T => Boolean)(implicit hint: ExecHint): Node[T, C] = filter(test)
  }
  case class NCreate[T, C <: Context](fac: () => T, hint: ExecHint) extends Node[T, C]
  case class NInput[T, C <: Context](key: C#InKey[T]) extends Node[T, C]
  case class NMap[S, T, C <: Context](src: Node[S, C], func: S => T, hint: ExecHint) extends Node[T, C]
  case class NFMap[S, T, C <: Context](src: Node[S, C], func: S => Node[T, C], hint: ExecHint) extends Node[T, C]
  case class NFilter[T, C <: Context](src: Node[T, C], test: T => Boolean, hint: ExecHint) extends Node[T, C]
  case class NExtern[T, C <: Context](sync: C#EvalSync => Option[T], async: C#EvalAsync => Fut[T], triggers: Seq[C#InKey[Any]]) extends Node[T, C]
}
*/