package com.phoenixkahlo.hellcraft.util

import java.io.OutputStream

class OutputStreamSplitter(targets: OutputStream*) extends OutputStream {
  override def write(b: Int): Unit = for (elem <- targets) { elem.write(b) }

  override def flush(): Unit = for (elem <- targets) { elem.flush() }

  override def close(): Unit = for (elem <- targets) { elem.close() }
}
