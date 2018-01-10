package com.phoenixkahlo.hellcraft.core.eval

import java.util.UUID
import java.util.concurrent.ThreadFactory
import java.util.function.Consumer

import com.phoenixkahlo.hellcraft.core.eval.Eval._
import com.phoenixkahlo.hellcraft.util.collections.spatial.SpatialTemporalQueue
import com.phoenixkahlo.hellcraft.util.collections.{Identity, IdentityKey, IdentityMap, TypeMatchingMap}
import com.phoenixkahlo.hellcraft.util.threading.{Fut, Never, UniExecutor}

import scala.collection.mutable.ArrayBuffer

class AsyncEval[T, C <: Eval.Context](root: Eval[T, C])(disposer: Option[Fut[T] => Unit] = None) {

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

  private def invalidate(in: TypeMatchingMap[C#InKey, Identity, Any]): Unit = {
    if (lastin.nonEmpty && (in ne lastin.get) && in != lastin.get) {
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

        val roots: Seq[N] = changed.flatMap(itriggers.getOrElse(_, Seq.empty)) ++ unknown.keySet

        roots.foldLeft(roots.to[Set])({ case (a, n) => a ++ traverse(n, a) })
      }

      // dispose of root
      val invalidatedRoot: Option[Fut[T]] =
        if (triggered contains iden(root)) Some(known(iden(root)).asInstanceOf[Fut[T]])
        else if (unknown contains iden(root)) Some(unknown(iden(root)).asInstanceOf[Fut[T]])
        else None
      for {
        disposer <- this.disposer
        disposee <- invalidatedRoot
      } {
        disposer(disposee)
        //println("invalidated")
      }

      // clear the triggered nodes
      known --= triggered

      // clear the unknown, they cannot be trusted
      unknown = Map.empty
    }
  }

  def fut(in: TypeMatchingMap[C#InKey, Identity, Any], pack: C#EvalAsync): Fut[T] = this.synchronized {
    // step 1: create new ID to kill phantom changes from last application
    applyID = UUID.randomUUID()

    // step 2: invalidate
    invalidate(in)

    // step 3: set the new last input
    lastin = Some(in)

    // step 4: reify the root, recursively reifying the whole monad graph as needed
    reify(root)(in, pack)
  }

  def query(in: TypeMatchingMap[C#InKey, Identity, Any], pack: C#EvalAsync): Option[T] =
    fut(in, pack).query

  def query(in: TypeMatchingMap[C#InKey, Identity, Any])(implicit unitIsPack: Unit => C#EvalAsync): Option[T] =
    query(in, unitIsPack(()))

  def weakFut(in: TypeMatchingMap[C#InKey, Identity, Any], pack: C#EvalAsync): Option[Fut[T]] = this.synchronized {
    if (lastin contains in) known.get(iden(root)).map(_.asInstanceOf[Fut[T]])
    else None
  }

  def weakQuery(in: TypeMatchingMap[C#InKey, Identity, Any], pack: C#EvalAsync): Option[T] =
    weakFut(in, pack).flatMap(_.query)

  def weakQuery(in: TypeMatchingMap[C#InKey, Identity, Any])(implicit unitIsPack: Unit => C#EvalAsync): Option[T] =
    weakQuery(in, unitIsPack(()))

  def dispose(): Unit = invalidate(TypeMatchingMap.empty[C#InKey, Identity, Any])

}


