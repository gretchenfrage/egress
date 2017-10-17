package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.badlogic.gdx.graphics.{GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.Renderable
import com.phoenixkahlo.hellcraft.graphics.shaders.GenericSID
import com.phoenixkahlo.hellcraft.graphics.{Interpolation, RenderUnit, ResourcePack}
import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.util.caches.{DisposableParamCache, ParamCache}
import com.phoenixkahlo.hellcraft.util.collections.ResourceNode
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

import scala.collection.mutable.ArrayBuffer

class Cloud(override val pos: V3F, val seed: Long, val size: V3F, override val id: UUID,
            @transient lastRenderer: CloudRenderer) extends Entity with Moveable {
  val renderer: CloudRenderer =
    if (lastRenderer != null) lastRenderer
    else new CloudRenderer(this)

  override def updatePos(newPos: V3F): Entity =
    new Cloud(newPos, seed, size, id, renderer)

  override def renderables(pack: ResourcePack): Seq[RenderUnit] =
    renderer(pack)
}

class CloudRenderer(cloud: Cloud) {
  // garbage-collectable vertex array that's computed in background
  val mesh = new ParamCache[ResourcePack, Fut[(Seq[Float], Seq[Short])]](pack => Fut({
    val verts = new ArrayBuffer[Float]
    var i = 0
    val indices = new ArrayBuffer[Short]



    (verts, indices)
  }, UniExecutor.exec(cloud.pos)))

  // resource-owning renderable that must be outputted as resource node whenever existing
  val renderable = new DisposableParamCache[ResourcePack, Renderable](pack => {
    val (verts, indices) = mesh(pack).await
    val m = new Mesh(true, verts.size, indices.size,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
      new VertexAttribute(Usage.TextureCoordinates, 2, "a_texCoord0"),
      new VertexAttribute(Usage.Normal, 3, "a_normal")
    )
    m.setVertices(verts.toArray)
    m.setIndices(indices.toArray)
    val r = new Renderable
    r.meshPart.mesh = m
    r.material = new com.badlogic.gdx.graphics.g3d.Material
    r.meshPart.offset = 0
    r.meshPart.size = indices.size
    r.meshPart.primitiveType = GL20.GL_TRIANGLES
    r.userData = GenericSID
    r
  }, _.meshPart.mesh.dispose())

  // garbage collectable render unit and associated resource. manages the renderable's state. the param cache is to
  // bind it to the resource pack and ensure there's only one of it.
  val units = new ParamCache[ResourcePack, Seq[RenderUnit]](pack => {
    val resource = new ResourceNode {
      override def dependencies: Seq[ResourceNode] = Seq.empty

      override def dispose(): Unit = renderable.invalidate
    }
    val unit = new RenderUnit {
      override def apply(interpolation: Interpolation): Seq[Renderable] = Seq(renderable(pack))

      override def resources: Seq[ResourceNode] = Seq(resource)
    }
    Seq(unit)
  })

  def apply(pack: ResourcePack): Seq[RenderUnit] = units(pack)
}