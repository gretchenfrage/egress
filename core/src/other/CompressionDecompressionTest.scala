package other

import com.phoenixkahlo.hellcraft.Chunk
import com.phoenixkahlo.hellcraft.util.{Origin, V3I}

/**
  * Created by kahlo on 6/12/2017.
  */
object CompressionDecompressionTest {

  def main(args: Array[String]): Unit = {
    val chunk = new Chunk(V3I(0, 0, 0), 16)
    for (i <- 0 until 4906) {
      if (i != chunk.compress(chunk.decompress(i)))
        println(i)
      else
        println("bueno")
    }
    for (v <- Origin until V3I(16, 16, 16)) {
      if (v != chunk.decompress(chunk.compress(v)))
        println(v)
      else
        println("bueno")
    }
    println("done")
  }

}
