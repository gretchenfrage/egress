package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.eval.{ExecSeq, WEval}
import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent, MakeRequest, PutEntity, RenderWorld, UpdateEffect, World}
import com.phoenixkahlo.hellcraft.core.request.{Request, Requested}
import com.phoenixkahlo.hellcraft.fgraphics.{Render, Shader}
import com.phoenixkahlo.hellcraft.math.{Origin, V3F, V3I}
import com.phoenixkahlo.hellcraft.util.caches.ParamCache

class RequestTester(override val id: UUID, request: Request[String]) extends Entity {
  override def pos: V3F = Origin

  def onComplete(result: Requested): Seq[UpdateEffect] = {
    for (str <- request.unlock(result)) {
      println(str)
    }
    Seq.empty
  }

  override def render(world: RenderWorld): Seq[Render[_ <: Shader]] = Seq.empty
}

case class OnComplete(result: Requested, entityID: UUID, eventID: UUID) extends ChunkEvent(Origin, eventID) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) =
    chunk -> chunk.entities(entityID).asInstanceOf[RequestTester].onComplete(result)
}

object RequestTester {
  implicit val exec = ExecSeq

  def apply(gen: => String, ids: Stream[UUID]): Seq[UpdateEffect] = {
    val request = Request[String](WEval(gen), ids.drop(0).head)
    val entity = new RequestTester(ids.drop(1).head, request)
    Seq(
      PutEntity(entity, ids.drop(2).head),
      MakeRequest(request, (result, world) => Seq(OnComplete(result, entity.id, ids.drop(3).head)))
    )
  }

  def chunkHash(p: V3I, ids: Stream[UUID]): Seq[UpdateEffect] = {
    val request = Request[String](WEval.chunk(p).map("hear it hurgling: " + _.pos.toString), ids.drop(0).head)
    val entity = new RequestTester(ids.drop(1).head, request)
    Seq(
      PutEntity(entity, ids.drop(2).head),
      MakeRequest(request, (result, world) => Seq(OnComplete(result, entity.id, ids.drop(3).head)))
    )
  }
}