package com.phoenixkahlo.hellcraft.core.eval

import java.util.UUID
import java.util.concurrent.ThreadFactory
import java.util.function.Consumer

import com.phoenixkahlo.hellcraft.core.eval.StateGraph._
import com.phoenixkahlo.hellcraft.util.collections.spatial.SpatialTemporalQueue
import com.phoenixkahlo.hellcraft.util.collections.{Identity, IdentityKey, IdentityMap, TypeMatchingMap}
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

import scala.collection.mutable.ArrayBuffer

class AsyncStateGraph[T, C <: StateGraph.Context](root: StateGraph.Node[T, C]) {
  private type Node[T] = StateGraph.Node[T, C]
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
      case ncreate@NCreate(fac, hint) =>
        // create will always be known and will be triggered by nothing
        // so it's like a memo func but with simpler locking mechanism
        known.get(IdentityKey(ncreate)) match {
          case Some(fut) => fut.asInstanceOf[Fut[T]]
          case None =>
            val fut = Fut(fac(), hint.exec(_)(pack.service))
            known += (IdentityKey(ncreate) -> fut)
            fut
        }
      case ninput@NInput(key: C#InKey[T]) =>
        // input will also always be known and will be triggered by input changes
        known.get(IdentityKey(ninput)) match {
          case Some(fut) => fut.asInstanceOf[Fut[T]]
          case None =>
            val fut = Fut(inputs(key), _.run())
            known = known + (iden(ninput) -> fut)
            itriggers += (key -> (itriggers.getOrElse(key, Seq.empty) :+ iden(ninput)))
            fut
        }
      case nmap: NMap[_, T, C] =>
        // map will always be known and will be triggered by its source
        known.get(IdentityKey(nmap)) match {
          case Some(fut) => fut.asInstanceOf[Fut[T]]
          case None =>
            // some helping the type system
            def f[S](nmap: NMap[S, T, C]): Fut[T] = {
              val fut = reify(nmap.src).map(nmap.func, nmap.hint.exec(_)(pack.service))
              known += (IdentityKey(nmap) -> fut)
              triggers += (IdentityKey(nmap.src) -> (triggers.getOrElse(IdentityKey(nmap.src), Seq.empty) :+ iden(nmap)))
              fut
            }
            f(nmap)
        }
      case nfmap: NFMap[_, T, C] =>
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
                def f[S](nfmap: NFMap[S, T, C]): Fut[T] = {
                  val fut0: Fut[Node[T]] = reify(nfmap.src).map(nfmap.func)
                  val fut1: Fut[T] = fut0.flatMap(reify(_))
                  unknown += (IdentityKey(nfmap) -> fut1)
                  // on initial mapping completion, transfer to known section, if ID is unchanged
                  val beforeID = applyID
                  fut0.onComplete(() => AsyncStateGraph.this.synchronized {
                    if (AsyncStateGraph.this.applyID == beforeID) {
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
      case nfilter@NFilter(src, test, hint) =>
        // filter will always be known and will be triggered by its source
        known.get(iden(nfilter)) match {
          case Some(fut) => fut.asInstanceOf[Fut[T]]
          case None =>
            val fut = reify(src).filter(test, hint.exec(_)(pack.service))
            known += (iden(nfilter) -> fut)
            triggers += (iden(src) -> (triggers.getOrElse(iden(src), Seq.empty) :+ iden(nfilter)))
            fut
        }
      case nextern@NExtern(sync, async, trigs) =>
        known.get(iden(nextern)) match {
          case Some(fut) => fut.asInstanceOf[Fut[T]]
          case None =>
            val fut = async(pack)
            known += (iden(nextern) -> fut)
            for (t <- trigs)
              itriggers += (t -> (itriggers.getOrElse(t, Seq.empty) :+ iden(nextern)))
            fut
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

    // step 3: set the new last input
    lastin = Some(in)

    // step 4: reify the root, recursively reifying the whole monad graph as needed
    reify(root)(in, pack)
  }

  def query(in: TypeMatchingMap[C#InKey, Identity, Any], pack: C#EvalAsync): Option[T] = fut(in, pack).query
}
/*
object StateGraphTest extends App {
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

  trait TestContext extends StateGraph.Context {
    override type InKey[+T] = IK[T]
  }

  val i1 = NInput[String, TestContext](K1)
  val i2 = NInput[String, TestContext](K2)
  val i3 = NInput[Int, TestContext](K3)
  val i4 = NInput[Int, TestContext](K4)

  /*
  val r = for {
    str1 <- i1
    str2 <- i2
    n <- i3
  } yield str1 + Stream.iterate("-")(identity).take(n).fold("")(_ + _) + str2
  */
  /*
  val r =
    i1.flatMap(i2.flatMap())
    */
  /*
  val i1i2 = i1.flatMap(a => i2.map(b => {
    println("ab computed")
    (a, b)
  }))
  val i3i4 = i3.flatMap(c => i4.map(d => {
    println("cd computed")
    (c, d)
  }))
  val r = i1i2.flatMap({ case (a, b) => i3i4.map({ case (c, d) => {
    println("r computed")
    a + Stream.iterate("-")(identity).take(c).fold("")(_ + _) + b + Stream.iterate("*")(identity).take(d).fold("")(_ + _)
  }})})
  */

  val graph = new StateGraph(r)

  var in = TypeMatchingMap[IK, Identity, Any](
    K1 -> "hello",
    K2 -> "world",
    K3 -> 5,
    K4 -> 7
  )
  val af = graph(in)
  val a = af.await
  println(a)

  in += (K3 -> 3)
  val bf = graph(in)
  val b = bf.await
  println(b)

  in += (K2 -> "hell")
  val cf = graph(in)

  in += (K1 -> "goodbye")
  val df = graph(in)

  println(df.await)
  println(cf.await)

  in = TypeMatchingMap[IK, Identity, Any](
    K1 -> "phoenix",
    K2 -> "kahlo",
    K3 -> 10,
    K4 -> 1
  )
  println(graph(in).await)

  UniExecutor.deactivate()
}
*/
object StateGraph {
  trait UniExecProvider {
    def service: UniExecutor
  }
  trait Context {
    type InKey[+T]
    type EvalAsync <: UniExecProvider
    type EvalSync
  }

  sealed trait Node[+T, C <: Context] {
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