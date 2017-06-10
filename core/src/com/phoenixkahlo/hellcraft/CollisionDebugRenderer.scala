package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.graphics.g3d.{Material, Renderable, RenderableProvider}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.util.{Cache, V3F}

import scala.collection.mutable

class CollisionDebugRenderer(cylinder: Cylinder) extends OutlineRenderer(Nil, Color.RED) {

  override def update(world: World): Unit = {
    blocks = cylinder.lastProcessed
    renderableCache.invalidate
  }

  override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit = {
    super.getRenderables(renderables, pool)
  }

}
