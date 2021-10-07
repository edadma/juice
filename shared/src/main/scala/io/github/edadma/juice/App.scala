package io.github.edadma.juice

import java.nio.file.{Files, Path}
import scala.collection.immutable.VectorMap
import scala.jdk.CollectionConverters._
import scala.language.postfixOps
import io.github.edadma.cross_platform.readFile
import io.github.edadma.squiggly.platformSpecific.yaml
import org.ekrich.config.{Config, ConfigFactory, ConfigParseOptions, ConfigSyntax}

import scala.annotation.tailrec

object App {

  val defaultProperties: Config = ConfigFactory.parseString(
    """
      |title =        Untitled
      |baseurl =      http://localhost:8000
      |languagecode = en-us
      |contentdir =   .
      |layoutdir =    .
      |shortcodedir = .
      |partialdir =   .
      |staticdir =    .
      |resourcedir =  resources
      |themesdir =    themes
      |cachedir =     /tmp/juice_cache
      |ignorefiles =
      |""".stripMargin,
    ConfigParseOptions.defaults.setSyntax(ConfigSyntax.PROPERTIES)
  )

  val run: PartialFunction[Command, Unit] = {
    case BuildCommand(src, dst) =>
      val conf = new ConfigWrapper(config(src))

      processDir(src, dst, conf)
    case ConfigCommand(src) => printConfig(config(src))
  }

  case class ContentFile(data: Any, content: String)

  def processDir(src: Path, dst: Path, conf: ConfigWrapper): Unit = {
    val md =
      listFiles(src, "md", "markdown") map { p =>
        val s = readFile(p.toString)
        val lines = scala.io.Source.fromString(s).getLines()
        val data =
          lines.next() match {
            case "---" =>
              val buf = new StringBuilder

              @tailrec
              def line(): Unit =
                if (lines.hasNext) {
                  lines.next() match {
                    case "---" =>
                    case s =>
                      buf ++= s
                      buf += '\n'
                      line()
                  }
                } else
                  problem(s"unexpected end of file which reading front matter: $p")

              line()
              buf.toString
            case _ => ""
          }

        ContentFile(yaml(data), lines map (_ :+ '\n') mkString)
      }

    println(md)
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

  def printConfig(c: Config): Unit =
    for ((k, v) <- configObject(c.root))
      println(s"$k = ${renderValue(v)}")

  def listFiles(dir: Path, exts: String*): List[Path] = {
    val suffixes = exts map ('.' +: _)

    Files.list(dir).iterator.asScala.toList filter (p =>
      isFile(p) && (suffixes.isEmpty || suffixes.exists(p.getFileName.toString endsWith _))) sortBy (_.getFileName.toString)
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

  def config(src: Path): Config =
    listFiles(src, "json", "conf", "properties", "props", "hocon").foldLeft(defaultProperties) {
      case (c, p) => readConfig(p) withFallback c
    }

}
