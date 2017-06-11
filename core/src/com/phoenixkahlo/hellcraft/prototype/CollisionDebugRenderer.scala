package com.phoenixkahlo.hellcraft.prototype

import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g3d.Renderable
import com.badlogic.gdx.utils.Pool

class CollisionDebugRenderer(cylinder: Cylinder) extends OutlineRenderer(Nil, Color.RED) {

  override def update(world: World): Unit = {
    blocks = cylinder.lastProcessed
    renderableCache.invalidate
  }

  override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit = {
    super.getRenderables(renderables, pool)
  }

}
