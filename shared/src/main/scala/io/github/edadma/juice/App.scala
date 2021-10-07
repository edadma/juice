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

  val run: PartialFunction[Command, Unit] = {
    case BuildCommand(src, dst) =>
      val src1 = src.normalize.toAbsolutePath
      val dst1 = (Option(dst) getOrElse (src1 resolve "public")).normalize.toAbsolutePath

      if (!isDir(src1)) problem(s"not a readable directory: $src1")
      if (!canCreate(dst1)) problem(s"not a writable directory: $dst1")

      if (!isDir(dst1))
        Files.createDirectory(dst1)

      val conf = new ConfigWrapper(config(src1, "basic"))

      println(processDir(src1, dst1))
    case ConfigCommand(src) =>
      println("Site config:")

      for ((k, v) <- configObject(config(src, "basic").root))
        println(s"  $k = ${renderValue(v)}")
  }

  case class ContentDir(name: String, dirs: List[ContentDir], contentFiles: List[ContentFile], data: Any)

  case class DataFile(name: String, data: Any)

  case class ContentFile(name: String, data: Any, content: String)

  def processDir(src: Path, dst: Path): ContentDir = {
    val dirs = listDirs(src, dst) map (processDir(_, dst))
    val mds =
      listFiles(src, "MD", "md", "markdown") map { p =>
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
                  problem(s"unexpected end of file while reading front matter: $p")

              line()
              buf.toString
            case _ => problem(s"expected front matter: $p")
          }

        ContentFile(withoutExtension(p.getFileName.toString), yaml(data), lines map (_ :+ '\n') mkString)
      }
    val data =
      listFiles(src, "YML", "YAML", "yml", "yaml") map { p =>
        withoutExtension(p.getFileName.toString) -> yaml(readFile(p.toString))
      } toMap

    ContentDir(withoutExtension(src.getFileName.toString), dirs, mds, data)
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

  def listFiles(dir: Path, exts: String*): List[Path] = {
    val suffixes = exts map ('.' +: _)

    Files.list(dir).iterator.asScala.toList filter (p =>
      isFile(p) && (suffixes.isEmpty || suffixes.exists(p.getFileName.toString endsWith _))) sortBy (_.getFileName.toString)
  }

  def listDirs(dir: Path, exclude: Path*): List[Path] =
    Files.list(dir).iterator.asScala.toList filter (p => !exclude.contains(p) && isDir(p))

  def extension(filename: String): String =
    filename lastIndexOf '.' match {
      case -1  => ""
      case dot => filename substring (dot + 1)
    }

  def withoutExtension(filename: String): String =
    filename lastIndexOf '.' match {
      case -1  => filename
      case dot => filename substring (0, dot)
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

  def config(src: Path, base: String): Config = {
    BaseConfig(base) match {
      case Some(b) =>
        listFiles(src, "json", "conf", "properties", "props", "hocon").foldLeft(b) {
          case (c, p) => readConfig(p) withFallback c
        }
      case None => problem(s"unknown base configuration: $base")
    }
  }

}
