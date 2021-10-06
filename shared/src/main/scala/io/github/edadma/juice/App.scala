package io.github.edadma.juice

import io.github.edadma.cross_platform.readFile

import org.ekrich.config.{Config, ConfigFactory, ConfigParseOptions, ConfigSyntax}

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._
import scala.language.postfixOps

object App {

  val defaultProperties: Config = ConfigFactory.parseString(
    """
      |title = Untitled
      |baseurl = http://localhost:8000
      |languagecode = en-us
      |contentdir = .
      |layoutdir = .
      |shortcodedir = .
      |partialdir = .
      |staticdir = .
      |resourcedir = resources
      |themesdir = themes
      |cachedir = /tmp/juice_cache
      |ignorefiles =
      |""".stripMargin,
    ConfigParseOptions.defaults.setSyntax(ConfigSyntax.PROPERTIES)
  )

  val run: PartialFunction[Command, Unit] = {
    case BuildCommand(src, dst) =>
      println(s"build src = $src, dst = $dst")
    case ConfigCommand(src) =>
  }

  def listFiles(dir: Path, exts: String*): List[Path] = {
    val suffixes = exts map (_ :+ '.')

    Files.list(dir).iterator.asScala filter (p =>
      isFile(p) && (suffixes.isEmpty || suffixes.exists(p.getFileName endsWith _))) toList
  }

  def extension(filename: String): String =
    filename lastIndexOf '.' match {
      case -1  => ""
      case dot => filename substring (dot + 1)
    }

  def readConfig(path: Path): Config = {
    val file = path.toString
    val syntax =
      extension(file) match {
        case "json" => ConfigParseOptions.defaults.setSyntax(ConfigSyntax.JSON)
      }
    ConfigFactory.parseString(readFile(file))
  }

  def config(src: Path): Config = {
    listFiles(src, "json", "conf", "properties", "props", "hocon").foldLeft(defaultProperties) {
      case (c, p) =>
    }
  }

}
