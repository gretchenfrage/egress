package com.phoenixkahlo.hellcraft.core.graphics

import java.io.{ByteArrayInputStream, DataInputStream}

import com.badlogic.gdx.Gdx
import com.phoenixkahlo.hellcraft.core.eval.{ExecSeq, GEval}
import com.phoenixkahlo.hellcraft.fgraphics.{BasicTriVert, Renderable, TerrainShader}
import com.phoenixkahlo.hellcraft.math.{V2F, V3F, V4F}

import scala.collection.mutable.ArrayBuffer

object Clouds {
  implicit val exec = ExecSeq

  val clouds: Seq[Renderable[TerrainShader]] =
    Stream.iterate(0)(_ + 1)
      .map(i => Gdx.files.internal("clouds/" + i + "_verts.dat") -> Gdx.files.internal("clouds/" + i + "_indices.dat"))
      .takeWhile({ case (f1, f2) => f1.exists() && f2.exists() })
      .map({ case (vertFile, indexFile) => {
        Renderable[TerrainShader](
          for {
            vertFileResult <- GEval.readFile(vertFile.file.toPath)
            indexFileResult <- GEval.readFile(indexFile.file.toPath)
          } yield (vertFileResult, indexFileResult) match {
            case (Left(vertData), Left(indexData)) =>
              val vin = new DataInputStream(new ByteArrayInputStream(vertData))
              val verts = new ArrayBuffer[BasicTriVert]
              while (vin.available() > 0)
                verts += BasicTriVert(
                  V3F(vin.readFloat(), vin.readFloat(), vin.readFloat()),
                  V4F.unpackColor(vin.readFloat()),
                  V2F(vin.readFloat(), vin.readFloat()),
                  V3F(vin.readFloat(), vin.readFloat(), vin.readFloat())
                )

              val iin = new DataInputStream(new ByteArrayInputStream(indexData))
              val indices = new ArrayBuffer[Short]
              while (iin.available() > 0)
                indices += iin.readShort()

              (verts, indices)
          }
        )
      }})

  def cloud(i: Int): Renderable[TerrainShader] = clouds(Math.abs(i) % clouds.size)
}
