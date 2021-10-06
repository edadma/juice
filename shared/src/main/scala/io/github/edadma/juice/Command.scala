package io.github.edadma.juice

import java.nio.file.{Path, Paths}

trait Command

case class BuildCommand(src: Path = Paths.get("."), dst: Path = null) extends Command

case class ServeCommand(src: Path = Paths.get("."), dst: Path = null) extends Command

case object ConfigCommand extends Command
