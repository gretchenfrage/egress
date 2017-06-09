package com.phoenixkahlo.hellcraft
import com.badlogic.gdx.graphics.g3d.Renderable
import com.badlogic.gdx.utils.{Array, Pool}
import com.phoenixkahlo.hellcraft.util.{Cache, V3I}

class OutlineRenderer(var blocks: Seq[V3I]) extends Entity {

  override def update(world: World): Unit = {}

  def buildRenderable: Renderable = ???

  val renderableCache = new Cache(buildRenderable)

  override def getRenderables(renderables: Array[Renderable], pool: Pool[Renderable]): Unit = {

  }
}
