package com.phoenixkahlo.hellcraft.singleplayer

import java.util.concurrent.ThreadLocalRandom

import com.phoenixkahlo.hellcraft.core.entity.{AnyEnt, AnyEntID}
import com.phoenixkahlo.hellcraft.core.event.UE
import com.phoenixkahlo.hellcraft.core.event.UE._
import com.phoenixkahlo.hellcraft.core.{Chunk, Event, LogEffect, MakeRequest, PutChunk, PutEnt, RemEnt, SoundEffect, Terrain, UpdateEffect}
import com.phoenixkahlo.hellcraft.math.{MRNG, V3I}
import com.phoenixkahlo.hellcraft.service.{ServiceProcedure, ServiceTagTable}
import com.phoenixkahlo.hellcraft.singleplayer.SingleWorld.{ChangeSummary, ChunkExchange, EntExchange}
import com.phoenixkahlo.hellcraft.util.collections.V3ISet
import com.phoenixkahlo.hellcraft.util.threading.{FirstToComplete, Fut, FutSeqFold, SettableFut}

import scala.collection.mutable

/*
trait ParWorld[W <: ParWorld[W]] {
  def chunkFut(p: V3I): Fut[Option[Chunk]]
  def terrainFut(p: V3I): Fut[Option[Terrain]]
  def findEntityFutUntyped(id: AnyEntID): Fut[Option[AnyEnt]]
  def cdomain: V3ISet
  def entEffects: Seq[UpdateEffect]
  def putChunks(cs: Seq[Chunk]): (W, Seq[ChunkExchange])
  def putEnt(ent: AnyEnt): (W, EntExchange)
  def remEnt(id: AnyEntID): (W, EntExchange)

  def parUpdate(time: Long, externs: Seq[UpdateEffect], services: ServiceTagTable[ServiceProcedure], exec: Runnable => Unit): (this.type, Seq[UpdateEffect], ChangeSummary) = {
    // immutable, future oriented state class
    case class State(
                      chunks: Map[V3I, Fut[Option[Chunk]]],
                      ents: Map[AnyEntID, Fut[Option[AnyEnt]]],
                      cexchanges: Vector[Fut[ChunkExchange]],
                      eexchanges: Vector[Fut[EntExchange]],
                      effout: Vector[UpdateEffect]
                    ) {
      def chunk(p: V3I): Fut[Option[Chunk]] = chunks.get(p) match {
        case Some(fut) => fut
        case None => ParWorld.this.chunkFut(p)
      }
      def terrain(p: V3I): Fut[Option[Terrain]] = chunks.get(p) match {
        case Some(fut) => fut.map(_.map(_.terrain), _.run())
        case None => ParWorld.this.terrainFut(p)
      }
      def ent(id: AnyEntID): Fut[Option[AnyEnt]] = ents.get(id) match {
        case Some(fut) => fut
        case None => ParWorld.this.findEntityFutUntyped(id)
      }

      def putc(p: V3I, c: Fut[Option[Chunk]]): State =
        copy(chunks = chunks + (p -> c), cexchanges = cexchanges :+ (for {
        before <- chunk(p)
        after <- c
      } yield ChunkExchange(before, after)))

      def pute(id: AnyEntID, e: Fut[Option[AnyEnt]]): State =
        copy(ents = ents + (id -> e), eexchanges = eexchanges :+ (for {
          before <- ent(id)
          after <- e
        } yield EntExchange(before, after)))

      def output(effect: UpdateEffect): State =
        copy(effout = effout :+ effect)
    }

    // advancer algebra for updating asynchronously and safely
    case class Advancer(func: State => (State, Seq[Fut[Advancer]]))
    def output(effect: UpdateEffect): Advancer = Advancer(state => (state.output(effect), Seq.empty))
    val nothing: Advancer = Advancer(state => (state, Seq.empty))

    def crawl(state: State, advancers: Set[Fut[Advancer]]): Fut[(State, Set[Advancer])] = {
      if (advancers.isEmpty) Fut((state, Set.empty), _.run())
      else FirstToComplete(advancers).flatMap({
        case (complete: Advancer, remaining: Set[Fut[Advancer]]) =>
          val (next: State, caused: Seq[Fut[Advancer]]) = complete.func(state)
          crawl(next, remaining ++ caused)
      })
    }

    // acquirements to be unlocked
    sealed trait Acq
    case class ChunkAcq(p: V3I, before: Fut[Option[Chunk]], settable: SettableFut[Option[Chunk]]) extends Acq
    case class EntAcq(id: AnyEntID, before: Fut[Option[AnyEnt]], settable: SettableFut[Option[AnyEnt]]) extends Acq

    // evaluate a UE monad using the aforementioned algebra
    def eval[T](ue: UE[T], onResult: (T, Seq[Acq]) => Advancer): Advancer = ue match {
      // world input monads (complicated, may require acquirement)
      case UEChunk(p) => Advancer(state => {
        val settable = new SettableFut[Option[Chunk]]
        val acq = ChunkAcq(p, state.chunk(p), settable)
        (state.putc(p, settable), Seq(state.chunk(p).map(chunk => onResult(chunk, Seq(acq)))))
      })
      case UEEnt(id) => Advancer(state => {
        val settable = new SettableFut[Option[AnyEnt]]
        val acq = EntAcq(id, state.ent(id), settable)
        (state.pute(id, settable), Seq(state.ent(id).map(ent => onResult(ent, Seq(acq)))))
      })
      case UETerrain(p) => Advancer(state => (state, Seq(Fut(onResult(state.terrain(p).asInstanceOf[T], Seq.empty), exec))))
      // simple input monads
      case UERand => onResult(new MRNG(ThreadLocalRandom.current.nextLong()), Seq.empty)
      case UETime => onResult(time, Seq.empty)
      // other monads
      case UEGen(fac) => onResult(fac(), Seq.empty)
      case ue: UEMap[_, T] =>
        def f[S](ue: UEMap[S, T]): Advancer =
          eval(ue.src, (t: S, acq) => onResult(ue.func(t), acq))
        f(ue)
      case ue: UEFMap[_, T] =>
        def f[S](ue: UEFMap[S, T]): Advancer =
          eval(ue.src, (s: S, acq1) => Advancer(state => (state, Seq(Fut(eval(ue.func(s), (t: T, acq2) => onResult(t, acq1 ++ acq2)), exec)))))
        f(ue)
      // just ignore filters, we don't really care about honoring them
      case UEFilter(src: UE[T], test: (T => Boolean)) => eval(src, onResult)
    }

    // now, recursively compile an effect into an advancer, using the previous function for update effects
    def process(effect: UpdateEffect): Advancer = effect match {
      case effect: SoundEffect => output(effect)
      case effect: MakeRequest[_] => output(effect)
      case effect: LogEffect => output(effect)
      case effect@PutChunk(chunk) =>
        if (cdomain contains chunk.pos)
          Advancer(state => (state.putc(chunk.pos, Fut(Some(chunk), _.run)), Seq.empty))
        else
          output(effect)
      case effect@PutEnt(ent) =>
        if (cdomain contains ent.chunkPos)
          // we use some evil callback retro trickery here
          Advancer(state => {
            val settable = new SettableFut[Option[AnyEnt]]
            (state.pute(ent.id, settable), Seq(state.chunk(ent.chunkPos).map({
              case Some(chunk) =>
                settable.set(Some(ent))
                nothing
              case None =>
                settable.set(None)
                output(effect)
            }, exec)))
          })
        else
          output(effect)
      case effect@RemEnt(id) =>
        // here we always remove, but asynchronously output the effect if the entity was never present
        Advancer(state => {
          (state.pute(id, Fut(None, _.run())), Seq(state.ent(id).map({
            case Some(ent) => nothing
            case None => output(effect)
          }, exec)))
        })
      case effect@Event(ue) => eval(ue, (caused: Seq[UpdateEffect], acquired: Seq[Acq]) => {
        val outEffects = new mutable.ArrayBuffer[UpdateEffect]
        var cMap: Map[V3I, Fut[Option[Chunk]]] = acquired.flatMap({
          case ChunkAcq(p, before, settable) => Some(p -> before)
          case _ => None
        }).toMap
        var eMap: Map[AnyEntID, Fut[Option[AnyEnt]]] = acquired.flatMap({
          case EntAcq(id, before, settable) => Some(id -> before): Option[(AnyEntID, Fut[Option[AnyEnt]])]
          case _ => None
        }).toMap
        caused foreach {
          case PutChunk(chunk) if cMap contains chunk.pos =>
            cMap += (chunk.pos -> Fut(Some(chunk), _.run()))
          case PutEnt(ent) if eMap contains ent.id =>
            eMap += (ent.id -> Fut(Some(ent), _.run()))
          case RemEnt(id) if eMap contains id =>
            eMap += (id -> Fut(None, _.run()))
          case other =>
            outEffects += other
        }
        acquired foreach {
          case ChunkAcq(p, before, settable) => settable.observe(cMap(p))
          case EntAcq(id, before, settable) => settable.observe(eMap(id))
        }
        Advancer(state => (state, outEffects.map(effect => Fut(output(effect), _.run()))))
      })
    }

    val startEffects: Seq[UpdateEffect] = externs ++ entEffects
    val initState: State = State(Map.empty, Map.empty, Vector.empty, Vector.empty, Vector.empty)
    val finalState: State = crawl(initState, startEffects.map(effect => Fut(process(effect), exec)).toSet).await._1
    val cexchanges: Seq[ChunkExchange] = FutSeqFold(finalState.cexchanges, exec).await
    val eexchanges: Seq[EntExchange] = FutSeqFold(finalState.eexchanges, exec).await
    val putChunks: Seq[Chunk] = FutSeqFold(finalState.chunks.values.toSeq, exec).await.flatten
    val putEnts: Map[AnyEntID, Option[AnyEnt]] =
      FutSeqFold(finalState.ents.toSeq.map({ case (id, fut) => fut.map(opt => (id, opt)) }), exec).await.toMap

    var world = this
    ;{
      val (w, c) = world.putChunks(putChunks)
      world = w
    }
    putEnts foreach {
      case (id, Some(ent)) =>
        val (w, c) = world putEnt ent
        world = w
      case (id, None) =>
        val (w, c) = world remEnt id
        world = w
    }

    (this, finalState.effout, ChangeSummary(cexchanges, eexchanges))
  }
}
*/