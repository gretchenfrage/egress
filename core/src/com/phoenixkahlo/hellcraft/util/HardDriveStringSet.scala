package com.phoenixkahlo.hellcraft.util

import java.io.{File, FileOutputStream, PrintStream}
import java.nio.file.Path
import java.util.Scanner

import scala.collection.parallel.mutable.ParSet
import scala.collection.{JavaConverters, mutable}

/**
  * A java mutable string set that mirrors a file on the hard drive containing one of the strings on each line.
  * As the file is modified, the modifications will be saved to the file automatically. When the set is
  * constructed, it reads its contents from the file if the file exists, and if not, it creates the file.
  * Instances of this are stored in a weak hash set where the keys are paths, ensuring that only one set
  * will be constructed for a file at a time. However, this is not inherently thread safe.
  */
class HardDriveStringSet private[this](file: File) extends java.util.HashSet[String] {

  def this(path: Path) = {
    this(path.toFile)
    if (!file.exists) file.createNewFile()
    val scanner = new Scanner(file)
    while (scanner.hasNextLine)
      add(scanner.nextLine())
  }

  private def save(): Unit = {
    val out = new PrintStream(file)
    for (str <- JavaConverters.collectionAsScalaIterable(this))
      out.println(str)
    out.close()
  }

  override def remove(o: scala.Any): Boolean = {
    try {
      super.remove(o)
    } finally {
      save()
    }
  }

  override def add(e: String): Boolean = {
    try {
      super.add(e)
    } finally {
      save()
    }
  }

  override def clear(): Unit = {
    try {
      super.clear()
    } finally {
      save()
    }
  }

}

object HardDriveStringSet {

  private val mutex = new Object
  private val map = new mutable.WeakHashMap[Path, HardDriveStringSet]

  def apply(path: Path): java.util.Set[String] = {
    mutex.synchronized {
      map.get(path) match {
        case Some(set) => set
        case None =>
          val set = new HardDriveStringSet(path)
          map.put(path, set)
          set
      }
    }
  }

}
