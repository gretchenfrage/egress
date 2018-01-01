package com.phoenixkahlo.hellcraft.core.eval

import com.phoenixkahlo.hellcraft.core.eval.Eval._
import com.phoenixkahlo.hellcraft.util.collections.{Identity, IdentityKey, TypeMatchingMap}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Phoenix on 12/31/2017.
  */
class SyncEval[T, C <: Eval.Context](root: Eval[T, C])(disposer: Option[T => Unit] = None) {

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

  private def invalidate(in: TypeMatchingMap[C#InKey, Identity, Any]): Unit = {
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

      // dispose of root
      for {
        disposer <- this.disposer
        disposee <- if (triggered(root)) Some(known(root).asInstanceOf[T]) else None
      } disposer(disposee)

      // clear the triggered nodes
      known --= triggered
    }
  }

  def query(in: TypeMatchingMap[C#InKey, Identity, Any], pack: C#EvalSync): Option[T] = {
    invalidate(in)

    lastin = Some(in)

    reify(root)(in, pack)
  }

  def query(in: TypeMatchingMap[C#InKey, Identity, Any])(implicit unitIsPack: Unit => C#EvalSync): Option[T] =
    query(in, unitIsPack(()))

  def dispose(): Unit = invalidate(TypeMatchingMap.empty[C#InKey, Identity, Any])

}
