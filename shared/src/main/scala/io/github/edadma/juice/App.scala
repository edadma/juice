package io.github.edadma.juice

import java.nio.file.{Files, Path, Paths}

object App {

  val run: PartialFunction[Command, Unit] = {
    case BuildCommand(src, dst) =>
      println(s"build src = $src, dst = $dst")
  }

}
