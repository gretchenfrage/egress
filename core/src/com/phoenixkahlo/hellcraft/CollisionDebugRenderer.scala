package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.graphics.g3d.{Renderable, RenderableProvider}
import com.badlogic.gdx.utils.{Array, Pool}

class CollisionDebugRenderer(cylinder: Cylinder) extends Entity {



  override def update(world: World): Unit = ???

  override def getRenderables(renderables: Array[Renderable], pool: Pool[Renderable]): Unit = ???


}
