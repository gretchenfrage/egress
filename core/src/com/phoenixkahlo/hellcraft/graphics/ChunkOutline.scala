package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g3d.{ModelInstance, Renderable}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.graphics.shaders.LineSID
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.collections.ResourceNode

import scala.collection.JavaConverters

class ChunkOutline(p: V3I, color: Color) extends RenderUnit {
  override def apply(interpolation: Interpolation): Seq[Renderable] = {
    // get model instance
    val instance = new ModelInstance(ChunkOutlineModel(color, color))
    instance.transform.setTranslation(p * 16 toGdx)
    // extract renderables from model
    val array = new com.badlogic.gdx.utils.Array[Renderable]()
    val pool = new Pool[Renderable]() {
      override def newObject(): Renderable = new Renderable
    }
    instance.getRenderables(array, pool)
    // set shader id
    for (renderable <- JavaConverters.asScalaIterator(array.iterator())) {
      renderable.userData = LineSID
    }
    // return renderables
    JavaConverters.iterableAsScalaIterable(array).toSeq
  }

  override def resources: Seq[ResourceNode] = Seq.empty
}
