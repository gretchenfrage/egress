package com.phoenixkahlo.hellcraft.graphics.old

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.model.MeshPart
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder
import com.badlogic.gdx.graphics.g3d.{Material, Model, ModelInstance, Renderable}
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.graphics.ChunkOutlineModel
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.oldcore.World
import com.phoenixkahlo.hellcraft.util.ResourceNode
import com.phoenixkahlo.hellcraft.util.caches.KeyParamPool

import scala.collection.JavaConverters

class ChunkOutlineRenderer(p: V3I, color: Color) extends RenderableFactory {
  /**
    * Bring this object into an active state, generating resources, and return the renderables. While activating,
    * interpolate with the other world.
    */
  override def apply(interpolate: Option[(World, Float)]): Seq[Renderable] = {
    // get model instance
    val instance = new ModelInstance(ChunkOutlineModel(color, color))
    instance.transform.setTranslation(p * 16 toGdx)
    // extract renderables from model
    val array = new com.badlogic.gdx.utils.Array[Renderable]()
    val pool = new Pool[Renderable]() {
      override def newObject(): Renderable = new Renderable
    }
    instance.getRenderables(array, pool)
    // return renderables
    JavaConverters.iterableAsScalaIterable(array).toSeq
  }

  override def resources: Seq[ResourceNode] = Seq.empty
}

