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

class ChunkRenderer(
                     chunk: Chunk,
                     texturePack: ResourcePack,
                     world: World,
                     previous: Option[ChunkRenderer]
                        ) extends RenderableFactory with ResourceNode {

  def compileProcedure: () => (Array[Float], Array[Short]) = () => {
    // first, check for heuristics
    if (chunk.blocks eq BlockGrid.AirGrid) (new Array[Float](0), new Array[Short](0))
    if ((chunk.blocks eq BlockGrid.StoneGrid) && {
      val adjacent = chunk.pos.touching.flatMap(world.weakChunkAt)
      adjacent.size == 6 && adjacent.forall(_.blocks eq BlockGrid.StoneGrid)
    }) (new Array[Float](0), new Array[Short](0))

    // first, compute the exposed surfaces
    type SurfaceMap = Map[Direction, List[V3I]]

    // compute the given surface of the given block
    def surface(m: SurfaceMap, v: V3I, s: Direction): SurfaceMap =
      (chunk(v % 16).get, world.weakBlockAt(v + s)) match {
        // if the target is translucent, the face is invisible
        case (t, _) if t isTranslucent => m
        // if the cover is opaque, the face is invisible
        case (_, Some(c)) if c isOpaque => m
        // if the cover is translucent (and the target is opaque), the face is visible
        case (_, Some(c)) if c isTranslucent => m.updated(s, v :: m(s))
        // if the cover is non-existent (and the target is opaque), the face is visible
        case (_, None) => m.updated(s, v :: m(s))
        // in all other cases, the face is invisible
        case _ => m
      }

    // compute all surfaces of a block
    def block(m: SurfaceMap, v: V3I): SurfaceMap =
      (Stream.iterate(v)(identity) zip Directions()).foldLeft(m)({ case (m, (v, s)) => surface(m, v, s) })

    // do the computation
    val empty: SurfaceMap = Directions() zip Stream.iterate(Nil)(identity) toMap
    val blocks: Seq[V3I] = (Origin until V3I(16, 16, 16)) map (_ + (chunk.pos * 16))
    val exposed: SurfaceMap = blocks.foldLeft(empty)(block)

    // fold the exposure sets into vertex data and indices
    type VertDatum = (V3F, Color, V2F)
    val vertSize = 6
    // convert from size in bytes to size in floats
    val p = 1
    val n = 0
    val offset = chunk.pos * 16

    def addSquareIndices(verts: List[VertDatum], indices: List[Short]): List[Short] =
      indices
        .::((verts.length + 0).toShort)
        .::((verts.length + 1).toShort)
        .::((verts.length + 2).toShort)
        .::((verts.length + 0).toShort)
        .::((verts.length + 2).toShort)
        .::((verts.length + 3).toShort)

    var data: (List[VertDatum], List[Short]) = (Nil, Nil)

    data = exposed(Up).foldLeft(data)(
      (data, b) => data match {
        case (verts, indices) =>
          val r = texturePack(world.blockAt(b).get.tid)
          (
            verts
              .::((b + V3F(n, p, p), Color.WHITE, V2F(r.getU, r.getV)))
              .::((b + V3F(p, p, p), Color.WHITE, V2F(r.getU2, r.getV)))
              .::((b + V3F(p, p, n), Color.WHITE, V2F(r.getU2, r.getV2)))
              .::((b + V3F(n, p, n), Color.WHITE, V2F(r.getU, r.getV2))),
            addSquareIndices(verts, indices)
          )
      }
    )
    data = exposed(West).foldLeft(data)(
      (data, b) => data match {
        case (verts, indices) =>
          val r = texturePack(world.blockAt(b).get.tid)
          (
            verts
              .::((b + V3F(n, p, p), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU, r.getV)))
              .::((b + V3F(n, p, n), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU2, r.getV)))
              .::((b + V3F(n, n, n), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU2, r.getV2)))
              .::((b + V3F(n, n, p), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU, r.getV2))),
            addSquareIndices(verts, indices)
          )
      }
    )
    data = exposed(East).foldLeft(data)(
      (data, b) => data match {
        case (verts, indices) =>
          val r = texturePack(world.blockAt(b).get.tid)
          (
            verts
              .::((b + V3F(p, p, n), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU, r.getV)))
              .::((b + V3F(p, p, p), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU2, r.getV)))
              .::((b + V3F(p, n, p), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU2, r.getV2)))
              .::((b + V3F(p, n, n), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU, r.getV2))),
            addSquareIndices(verts, indices)
          )
      }
    )
    data = exposed(South).foldLeft(data)(
      (data, b) => data match {
        case (verts, indices) =>
          val r = texturePack(world.blockAt(b).get.tid)
          (
            verts
              .::((b + V3F(n, n, n), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU, r.getV)))
              .::((b + V3F(n, p, n), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU2, r.getV)))
              .::((b + V3F(p, p, n), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU2, r.getV2)))
              .::((b + V3F(p, n, n), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU, r.getV2))),
            addSquareIndices(verts, indices)
          )
      }
    )
    data = exposed(North).foldLeft(data)(
      (data, b) => data match {
        case (verts, indices) =>
          val r = texturePack(world.blockAt(b).get.tid)
          (
            verts
              .::((b + V3F(n, n, p), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU, r.getV)))
              .::((b + V3F(p, n, p), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU2, r.getV)))
              .::((b + V3F(p, p, p), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU2, r.getV2)))
              .::((b + V3F(n, p, p), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU, r.getV2))),
            addSquareIndices(verts, indices)
          )
      }
    )
    data = exposed(Down).foldLeft(data)(
      (data, b) => data match {
        case (verts, indices) =>
          val r = texturePack(world.blockAt(b).get.tid)
          (
            verts
              .::((b + V3F(n, n, p), new Color(0.75f, 0.75f, 0.75f, 1f), V2F(r.getU, r.getV)))
              .::((b + V3F(n, n, n), new Color(0.75f, 0.75f, 0.75f, 1f), V2F(r.getU2, r.getV)))
              .::((b + V3F(p, n, n), new Color(0.75f, 0.75f, 0.75f, 1f), V2F(r.getU2, r.getV2)))
              .::((b + V3F(p, n, p), new Color(0.75f, 0.75f, 0.75f, 1f), V2F(r.getU, r.getV2))),
            addSquareIndices(verts, indices)
          )
      }
    )

    val (vertices: List[VertDatum], indices: List[Short]) = data

    // reverse and serialize the vertex data into floats
    val vertexSerial = vertices.reverse.flatMap({
      case (v, color, t) => List(
        v.x, v.y, v.z,
        color toFloatBits,
        t.x, t.y
      )
    })


    // compile the vertices into an array (they were already reversed during serialization)
    val vertArr = new Array[Float](vertexSerial.size)
    var i = 0
    for (f <- vertexSerial) {
      vertArr.update(i, f)
      i += 1
    }
    // reverse and compile the indices into an array
    val indexArr = new Array[Short](indices.size)
    i = 0
    for (s <- indices.reverseIterator) {
      indexArr.update(i, s)
      i += 1
    }

    (vertArr, indexArr)
  }

  /*
  val meshData: Fut[(Array[Float], Array[Short])] =
    if (previous isDefined) {
      println("compiling in foreground!")
      Fut(compileProcedure(), _.run())
    } else Fut(compileProcedure(), UniExecutor.mesh(chunk.pos * 16 + V3I(8, 8, 8)))
    */

  def isoCompile(): (Array[Float], Array[Short]) = {
    val density: V3F => Float = v => {
      def densityOf(b: Block) = if (b isOpaque) 1f else 0f
      /*
      chunk.blocks(v.floor).map(densityOf)
        .getOrElse(world.blockAt(v.floor + (chunk.pos * 16)).map(densityOf).getOrElse({
          println("mesh compiler defaulting")
          0f
        }))
        */
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
    //val maxVerts = 4 * 6 * 4096
    //val maxIndices = 6 * 6 * 4096

    // get the arrays
    val (vertArr, indexArr) = isoMeshData.await

    val maxVerts = vertArr.length
    val maxIndices = indexArr.length

    // create a mesh
    val mesh = new Mesh(true, maxVerts, maxIndices,
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
