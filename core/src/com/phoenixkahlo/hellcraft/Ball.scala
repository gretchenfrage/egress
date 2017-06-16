package com.phoenixkahlo.hellcraft
import java.util.UUID

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.TextureAttribute
import com.badlogic.gdx.graphics.g3d.{Material, ModelInstance, Renderable}
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.util.V3F

import scala.collection.JavaConverters

case class Ball(
               pos: V3F,
               id: UUID = UUID.randomUUID()
               ) extends Entity {

  override def update(world: World): Seq[ChunkEvent] = Nil

  override def renderables(texturePack: TexturePack): Seq[RenderableFactory] =
    Seq(BallRenderer(this, texturePack))

}

case class BallRenderer(ball: Ball, texturePack: TexturePack) extends RenderableFactory {

  lazy val model: ModelInstance = {
    val builder = new ModelBuilder
    val material = new Material(
      TextureAttribute.createDiffuse(texturePack.apply(SandTID))
    )
    val template = builder.createSphere(1, 1, 1, 24, 24, material, Usage.Position | Usage.TextureCoordinates)
    new ModelInstance(template)
  }

  override def apply(): Seq[Renderable] = {
    val array = new com.badlogic.gdx.utils.Array[Renderable]()
    val pool = new Pool[Renderable]() { override def newObject(): Renderable = new Renderable }
    model.getRenderables(array, pool)
    JavaConverters.iterableAsScalaIterable(array).toSeq
  }

}
