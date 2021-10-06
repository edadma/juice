package io.github.edadma.juice

import io.github.edadma.cross_platform.readFile
import org.ekrich.config.impl.{ConfigBoolean, ConfigNumber, ConfigString}
import org.ekrich.config.{
  Config,
  ConfigFactory,
  ConfigList,
  ConfigObject,
  ConfigParseOptions,
  ConfigSyntax,
  ConfigValue
}

import java.nio.file.{Files, Path, Paths}
import scala.collection.immutable.VectorMap
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
      printConfig(config(src))
  }

  def configValue(v: ConfigValue): Any =
    v match {
      case s: ConfigString  => s.unwrapped
      case b: ConfigBoolean => b.unwrapped
      case n: ConfigNumber =>
        n.unwrapped match {
          case n: java.lang.Integer => n.intValue
          case n: java.lang.Double  => n.doubleValue
        }
      case o: ConfigObject => configObject(o)
      case l: ConfigList   => l.asScala map configValue
    }

  def renderValue(v: Any): String =
    v match {
      case s: String          => s"${'"'}$s${'"'}"
      case n: Int             => n.toString
      case n: Double          => n.toString
      case b: Boolean         => b.toString
      case l: List[_]         => l map renderValue mkString ("[", ", ", "]")
      case m: VectorMap[_, _] => m map { case (k, v) => s"$k: ${renderValue(v)}" } mkString ("{", ", ", "}")
    }

  def configObject(o: ConfigObject): VectorMap[String, Any] =
    o.entrySet.asScala.toList map (e => e.getKey -> configValue(e.getValue)) sortBy (_._1) to VectorMap

  def printConfig(c: Config): Unit =
    for ((k, v) <- configObject(c.root))
      println(s"$k = ${renderValue(v)}")

  def listFiles(dir: Path, exts: String*): List[Path] = {
    val suffixes = exts map ('.' +: _)

    Files.list(dir).iterator.asScala.toList filter (p =>
      isFile(p) && (suffixes.isEmpty || suffixes.exists(p.getFileName.toString endsWith _)))
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
        case "json"                 => ConfigParseOptions.defaults.setSyntax(ConfigSyntax.JSON)
        case "conf" | "hocon"       => ConfigParseOptions.defaults.setSyntax(ConfigSyntax.CONF)
        case "props" | "properties" => ConfigParseOptions.defaults.setSyntax(ConfigSyntax.PROPERTIES)
      }
    ConfigFactory.parseString(readFile(file), syntax)
  }

  def config(src: Path): Config = {
    listFiles(src, "json", "conf", "properties", "props", "hocon").foldLeft(defaultProperties) {
      case (c, p) => readConfig(p) withFallback c
    }
  }

}
