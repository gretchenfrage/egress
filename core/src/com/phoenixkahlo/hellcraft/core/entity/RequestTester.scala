package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.eval.{ExecSeq, WEval}
import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent, MakeRequest, RenderWorld, UniChunkEvent, UpdateEffect, World}
import com.phoenixkahlo.hellcraft.core.request.{Request, Requested}
import com.phoenixkahlo.hellcraft.fgraphics.{Render, Shader}
import com.phoenixkahlo.hellcraft.math.{MRNG, Origin, V3F, V3I}
import com.phoenixkahlo.hellcraft.util.caches.ParamCache

/*
class RequestTester(override val id: EntityID[RequestTester], request: Request[String]) extends Entity {

  override def pos: V3F = Origin

  def onComplete(result: Requested): Seq[UpdateEffect] = {
    for (str <- request.unlock(result)) {
      println(str)
    }
    Seq.empty
  }

  override def render(world: RenderWorld): Seq[Render[_ <: Shader]] = Seq.empty
}

**/
/*
case class OnComplete(result: Requested, entityID: UUID, eventID: UUID) extends ChunkEvent(Origin, eventID) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) =
    chunk -> chunk.entities(entityID).asInstanceOf[RequestTester].onComplete(result)
}
*/

/*
object OnComplete {
  def apply(result: Requested, entID: EntityID[RequestTester], rand: MRNG): ChunkEvent =
    UniChunkEvent(Origin, (chunk, world) => (chunk, chunk.entities(entID).asInstanceOf[RequestTester].onComplete(result)), rand.nextUUID)
}

object RequestTester {
  implicit val exec = ExecSeq

  def apply(gen: => String, ids: Stream[UUID])(implicit rand: MRNG): Seq[UpdateEffect] = {
    val request = Request[String](WEval(gen), ids.drop(0).head)
    val entity: RequestTester = new RequestTester(EntityID(), request)
    Seq(
      ChunkEvent.putEnt(entity),
      MakeRequest(request, (result, world, rand) => Seq(OnComplete(result, entity.id, rand)))
    )
  }

  def chunkHash(p: V3I, ids: Stream[UUID])(implicit rand: MRNG): Seq[UpdateEffect] = {
    val request = Request[String](WEval.chunk(p).map("hear it hurgling: " + _.pos.toString), ids.drop(0).head)
    val entity = new RequestTester(ids.drop(1).head, request)
    Seq(
      ChunkEvent.putEnt(entity),
      MakeRequest(request, (result, world, rand) => Seq(OnComplete(result, entity.id, rand)))
    )
  }
}
*/