package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.Renderable
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.phoenixkahlo.hellcraft.graphics.shaders.ParticleSID
import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.util.collections.ResourceNode

import scala.collection.mutable.ArrayBuffer

case class ParticleType(size: Float, tex: SheetTextureID, col: Color = Color.WHITE)

object ParticleFactory {
  def apply(resourcePack: ResourcePack, particles: (ParticleType, V3F)*): RenderUnit = {
    val vertexData = new ArrayBuffer[Float](particles.size * 9)
    for ((particle, pos) <- particles) {
      val tex = resourcePack.sheetRegion(particle.tex)
      vertexData.append(
        pos.x, pos.y, pos.z,
        particle.col.toFloatBits,
        particle.size,
        tex.getU, tex.getV,
        tex.getU2, tex.getV2
      )
    }
    val indexData = 0 until particles.size map (_ toShort)
    val mesh = new Mesh(true, vertexData.length, vertexData.length,
      new VertexAttribute(Usage.Generic, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
      new VertexAttribute(Usage.Generic, 1, "a_size"),
      new VertexAttribute(Usage.Generic, 2, "a_texCoord0"),
      new VertexAttribute(Usage.Generic, 2, "a_texCoord1")
    )
    mesh.setVertices(vertexData.toArray)
    mesh.setIndices(indexData.toArray)

    val renderable = new Renderable
    renderable.meshPart.mesh = mesh
    renderable.material = new com.badlogic.gdx.graphics.g3d.Material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = indexData.size
    renderable.meshPart.primitiveType = GL20.GL_POINTS
    renderable.userData = ParticleSID

    val resource = new ResourceNode {
      override def dependencies: Seq[ResourceNode] = Seq.empty

      override def dispose(): Unit = mesh.dispose()
    }

    new RenderUnit {
      override def apply(interpolation: Interpolation): Seq[Renderable] = Seq(renderable)

      override def resources: Seq[ResourceNode] = Seq(resource)
    }
  }
}

