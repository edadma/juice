package io.github.edadma

import java.nio.file.{Files, Path}

package object juice {

  def isFile(p: Path): Boolean = Files.isRegularFile(p) && Files.isReadable(p)

  def isDir(p: Path): Boolean = Files.isDirectory(p) && Files.isReadable(p)

  def canCreate(p: Path): Boolean = Files.isDirectory(p.getParent) && Files.isWritable(p.getParent)

  def problem(msg: String): Nothing = {
    Console.err.println(msg)
    sys.exit(1)
  }

}
