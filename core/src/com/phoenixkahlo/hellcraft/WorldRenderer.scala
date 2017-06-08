package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.{Gdx, utils}
import com.badlogic.gdx.graphics.Texture
import com.badlogic.gdx.graphics.g3d.{Renderable, RenderableProvider}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.util.Origin

class WorldRenderer(val world: World) extends RenderableProvider {

  val textures = new Texture(Gdx.files.internal("blocks.png"))
  val renderers =
    for {
      v <- Origin until world.size
    } yield {
      new ChunkRenderer(world.chunk(v).get, v * world.chunkSize, textures)
    }

  override def getRenderables(renderables: utils.Array[Renderable], pool: Pool[Renderable]): Unit = {
    renderers.foreach(_.getRenderables(renderables, pool))
  }
}
