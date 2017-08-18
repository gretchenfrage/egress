package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.{ColorAttribute, TextureAttribute}
import com.badlogic.gdx.graphics.g3d.model.MeshPart
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder
import com.badlogic.gdx.graphics.g3d.{Material, Model, ModelInstance, Renderable}
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.math.isosurface.SurfaceNets
import com.phoenixkahlo.hellcraft.threading.{Fut, UniExecutor}
import com.phoenixkahlo.hellcraft.util._

import scala.collection.JavaConverters

class ChunkRenderer(chunk: Chunk, compiler: Runnable => Unit) {

  private val mesh = new ParamCache[(World, ResourcePack), RenderUnit]({ case (world, pack) => {
    ???
  }})

  def apply(world: World, pack: ResourcePack): Seq[RenderUnit] = {
    if (chunk.pos.neighbors.forall(world.chunkAt(_).isDefined)) {
      Seq(mesh((world, pack)))
    } else Seq.empty
  }

}

/*
class ChunkRenderer(
                     chunk: Chunk,
                     texturePack: ResourcePack,
                     world: World,
                     previous: Option[ChunkRenderer]
                        ) extends RenderableFactory with ResourceNode {


  def isoCompile(): (Array[Float], Array[Short]) = {
    val density: V3F => Float = v => {
      def densityOf(b: Block) = if (b isOpaque) 1f else 0f
      densityOf(world.blockAt(v.floor + (chunk.pos * 16)).get)
    }
    val quads = SurfaceNets(V3I(18, 18, 18), density).map(_.map(_ + (chunk.pos * 16)))
    QuadCompiler(quads, texturePack)
  }

  val isoMeshData: Fut[(Array[Float], Array[Short])] =
    if (previous isDefined) {
      Fut(isoCompile(), _.run())
    } else Fut(isoCompile(), UniExecutor.mesh(chunk.pos * 16 + V3I(8, 8, 8)))

  val renderable = new DisposableCache[Renderable]({
    // get the arrays
    val (vertArr, indexArr) = isoMeshData.await

    // create a mesh
    val mesh = new Mesh(true, vertArr.size, indexArr.size,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
      new VertexAttribute(Usage.TextureCoordinates, 2, "a_texCoord0")
    )

    // plug the arrays into the mesh (this uploads them to VRAM)
    mesh.setVertices(vertArr)
    mesh.setIndices(indexArr)

    // create the material
    val material = new Material
    material.set(TextureAttribute.createDiffuse(texturePack.sheet))

    // create the renderable
    val renderable = new Renderable()
    renderable.meshPart.mesh = mesh
    renderable.material = material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = indexArr.length
    renderable.meshPart.primitiveType = GL20.GL_TRIANGLES
    renderable
  }, _.meshPart.mesh.dispose())

  /**
    * Bring this object into an active state, generating resources, and return the renderables.
    */
  override def apply(interpolate: Option[(World, Float)]): Seq[Renderable] =
    if (isoMeshData.query.isDefined) renderable() +: {
      if (Gdx.input.isKeyPressed(Keys.ALT_LEFT)) {
        // get model instance
        val instance = new ModelInstance(ChunkOutlineModel(Color.GREEN, Color.GREEN))
        instance.transform.setTranslation(chunk.pos * 16 toGdx)
        // extract renderables from model
        val array = new com.badlogic.gdx.utils.Array[Renderable]()
        val pool = new Pool[Renderable]() {
          override def newObject(): Renderable = new Renderable
        }
        instance.getRenderables(array, pool)
        // return renderables
        JavaConverters.iterableAsScalaIterable(array).toSeq
      } else Seq.empty
    }
    else previous match {
      case Some(renderer) => renderer()
      case None => if (Gdx.input.isKeyPressed(Keys.ALT_LEFT)) {
        // get model instance
        val instance = new ModelInstance(ChunkOutlineModel(Color.RED, Color.RED))
        instance.transform.setTranslation(chunk.pos * 16 toGdx)
        // extract renderables from model
        val array = new com.badlogic.gdx.utils.Array[Renderable]()
        val pool = new Pool[Renderable]() {
          override def newObject(): Renderable = new Renderable
        }
        instance.getRenderables(array, pool)
        // return renderables
        JavaConverters.iterableAsScalaIterable(array).toSeq
      } else Seq.empty
    }

  override def resources: Seq[ResourceNode] = Seq(this)

  override def dependencies: Seq[ResourceNode] =
    if (isoMeshData.query.isDefined) Nil
    else previous match {
      case Some(previous) => Seq(previous)
      case None => Nil
    }

  override def dispose(): Unit = {
    renderable.invalidate
  }

}

*/