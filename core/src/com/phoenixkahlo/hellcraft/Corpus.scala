package com.phoenixkahlo.hellcraft

import java.util.UUID

import com.badlogic.gdx.graphics.g3d.{Model, ModelInstance, Renderable}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.util.{KeyParamPool, V3F, V3I}

import scala.collection.{JavaConverters, mutable}

/**
  * An abstract entity that provides a framework for things pertaining to an entity being a moveable physical object.
  * Update is implemented, but a function is declared to transform itself. Corpus has a position property, and if the
  * self transformation moves the corpus into a different chunk, the update method will transfer the corpus between
  * chunks. The corpus can produce a modelID and a modelFactory, and if it produces the same modelID on multiple tics,
  * the generated model will be reused, after settings the translation to the position plus a model offset.
  */
abstract class Corpus(
                          val pos: V3F,
                          val id: UUID,
                          val chunkSize: Int = 16
                        ) extends Entity {


  protected def transform(world: World): Corpus

  protected def modelID: UUID = id

  protected def modelOffset: V3F

  protected def modelFactory(texturePack: TexturePack): ModelInstanceFactory

  lazy val chunkPos: V3I = pos / chunkSize floor

  override def update(world: World): Seq[ChunkEvent] = {
    val replacer = transform(world)
    if (replacer.chunkPos == this.chunkPos)
      Seq(ChunkEvent(chunkPos, _.putEntity(replacer)))
    else
      Seq(
        ChunkEvent(chunkPos, _.removeEntity(this)),
        ChunkEvent(replacer.chunkPos, _.putEntity(replacer))
      )
  }

  override def renderables(texturePack: TexturePack): Seq[RenderableFactory] =
    Seq(new RenderableFactory {
      override def apply(): Seq[Renderable] = {
        // obtain model
        val model = ModelPool(modelID, modelFactory(texturePack))
        // transform model
        model.transform.setTranslation(pos + modelOffset toGdx)
        // extract renderables from model
        val array = new com.badlogic.gdx.utils.Array[Renderable]()
        val pool = new Pool[Renderable]() {
          override def newObject(): Renderable = new Renderable
        }
        model.getRenderables(array, pool)
        // render renderables
        JavaConverters.iterableAsScalaIterable(array).toSeq
      }
    })

}

trait ModelInstanceFactory { def apply(): ModelInstance }

object ModelPool extends KeyParamPool[UUID,ModelInstanceFactory,ModelInstance](_())
