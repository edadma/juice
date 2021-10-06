package io.github.edadma.juice

import org.ekrich.config.{Config, ConfigFactory, ConfigParseOptions, ConfigSyntax}

import java.nio.file.{Files, Path, Paths}

object App {

  val defaultProperties: Config = ConfigFactory.parseString(
    """
      |baseurl = http://localhost:8000
      |layouts = .
      |shortcodes = .
      |partials = .
      |""".stripMargin,
    ConfigParseOptions.defaults.setSyntax(ConfigSyntax.PROPERTIES)
  )

  val run: PartialFunction[Command, Unit] = {
    case BuildCommand(src, dst) =>
      println(s"build src = $src, dst = $dst")
    case ConfigCommand(src) => println(s"config: $src")
  }

}
