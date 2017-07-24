package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.badlogic.gdx.graphics.g3d.{ModelInstance, Renderable}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}
import com.phoenixkahlo.hellcraft.util.KeyParamPool

import scala.collection.JavaConverters
import scala.collection.immutable.TreeSet

/**
  * An abstract entity that provides a framework for things pertaining to an entity being a moveable physical object.
  * Update is implemented, but a function is declared to transform itself. Corpus has a position property, and if the
  * self transformation moves the corpus into a different chunk, the update method will transfer the corpus between
  * chunks. The corpus can produce a modelID and a modelFactory, and if it produces the same modelID on multiple tics,
  * the generated model will be reused, after settings the translation to the position plus a model offset.
  */
//TODO: allow for dynamic model disposal
abstract class Corpus(
                          val pos: V3F,
                          val id: UUID,
                          val chunkSize: Int = 16
                        ) extends PositionHaver {


  protected def transform(world: World): Corpus

  def modelID: UUID = id

  def modelOffset: V3F

  def modelFactory(texturePack: TexturePack): ModelInstanceFactory

  override lazy val chunkPos: V3I = pos / chunkSize floor

  override def update(world: World, ids: Stream[UUID]): Seq[ChunkEvent] = {
    val replacer = transform(world)

    if (replacer.chunkPos == this.chunkPos)
      Seq(ReplaceEntity(replacer, ids.head))
      //Seq(AddEntity(replacer, ids.head))
    else
      Seq(
        RemoveEntity(this, ids.head),
        AddEntity(replacer, ids.drop(1).head)
      )
  }

  override def renderables(texturePack: TexturePack): Seq[RenderableFactory] =
    Seq(PooledInstanceRenderer(this, texturePack))

  def interpolatePos(world: World, fraction: Float): V3F =
    world.findEntity(id).map(_.asInstanceOf[Corpus].pos) match {
      case Some(pos2) => ((pos2 - pos) * fraction) + pos
      case None => pos
    }

}

case class PooledInstanceRenderer(corpus: Corpus, texturePack: TexturePack) extends RenderableFactory {

  /**
    * Bring this object into an active state, generating resources, and return the renderables.
    */
  override def apply(interpolate: Option[(World, Float)]): Seq[Renderable] = {
    // obtain model
    val model = ModelPool(corpus.modelID, corpus.modelFactory(texturePack))
    // interpolate
    val translate = (interpolate match {
      case Some((world, fraction)) => corpus.interpolatePos(world, fraction)
      case None => corpus.pos
    }) + corpus.modelOffset
    // translate model
    //model.transform.setTranslation(corpus.pos + corpus.modelOffset toGdx)
    model.transform.setTranslation(translate toGdx)
    // extract renderables from model
    val array = new com.badlogic.gdx.utils.Array[Renderable]()
    val pool = new Pool[Renderable]() {
      override def newObject(): Renderable = new Renderable
    }
    model.getRenderables(array, pool)
    // return renderables
    JavaConverters.iterableAsScalaIterable(array).toSeq
  }

  override def resources: Seq[ResourceNode] = Nil

}

trait ModelInstanceFactory { def apply(): ModelInstance }

object ModelPool extends KeyParamPool[UUID, ModelInstanceFactory, ModelInstance](_())