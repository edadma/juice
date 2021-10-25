package io.github.edadma

import io.github.edadma.commonmark.CommonMarkParser
import io.github.edadma.squiggly.{TemplateBuiltin, TemplateParser}
import org.ekrich.config.impl.{ConfigBoolean, ConfigNumber, ConfigString}
import org.ekrich.config.{ConfigList, ConfigObject, ConfigValue}

import java.nio.file.{Files, Path}
import scala.collection.immutable.VectorMap
import scala.jdk.CollectionConverters._
import scala.language.postfixOps
import scala.util.matching.Regex

package object juicer {

  def isFile(p: Path): Boolean = Files.isRegularFile(p) && Files.isReadable(p)

  def isDir(p: Path): Boolean = Files.isDirectory(p) && Files.isReadable(p)

  def canCreate(p: Path): Boolean = Files.isDirectory(p.getParent) && Files.isWritable(p.getParent)

  def problem(msg: String): Nothing = {
    Console.err.println(msg)
    sys.exit(1)
  }

  def configList(l: ConfigList): List[Any] = l.asScala map configValue toList

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
      case l: ConfigList   => configList(l)
    }

  def configObject(o: ConfigObject): VectorMap[String, Any] =
    o.entrySet.asScala.toList map (e => e.getKey -> configValue(e.getValue)) sortBy (_._1) to VectorMap

  val BaseURLRegex: Regex = raw"(http(?:s)?://[a-zA-Z0-9-.]+(?::\d+)?|file://)?((?:/[a-zA-Z0-9-.]+)*/?)".r

  case class BaseURL(base: String, path: String)

  def parseurl(s: String): Option[BaseURL] =
    s match {
      case BaseURLRegex(base, path) =>
        Some(
          BaseURL(if (base eq null) "" else base,
                  if (path == "") "/"
                  else if (path endsWith "/") path dropRight 1
                  else path))
      case _ => None
    }

  case class Args(config: String = "standard",
                  verbose: Boolean = false,
                  baseurl: Option[String] = None,
                  cmd: Option[Command] = None)

  var showSteps = false

  def show(msg: String): Unit = if (showSteps) println(msg)

  def list(dir: Path): List[Path] = Files.list(dir).iterator.asScala.toList

  def includeExts(listing: List[Path], exts: String*): List[Path] = {
    val suffixes = exts map ('.' +: _)

    listing filter (p => isFile(p) && (suffixes.isEmpty || suffixes.exists(p.getFileName.toString endsWith _))) sortBy (_.getFileName.toString)
  }

  def excludeExts(listing: List[Path], exts: String*): List[Path] = {
    require(exts.nonEmpty)

    val suffixes = exts map ('.' +: _)

    listing filter (p => isFile(p) && (!suffixes.exists(p.getFileName.toString endsWith _)))
  }

  def excludeDirs(listing: List[Path], exclude: Path*): List[Path] =
    listing filter (p => isDir(p) && !exclude.contains(p))

  lazy val templateFunctions
    : Map[String, squiggly.TemplateFunction] = TemplateBuiltin.functions ++ JuicerBuiltin.functions
  lazy val templateParser: TemplateParser = new TemplateParser(functions = templateFunctions)
  lazy val markdownParser = new CommonMarkParser

}
