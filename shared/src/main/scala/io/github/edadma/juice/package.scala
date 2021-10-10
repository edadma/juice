package io.github.edadma

import org.ekrich.config.impl.{ConfigBoolean, ConfigNumber, ConfigString}
import org.ekrich.config.{ConfigList, ConfigObject, ConfigValue}

import java.nio.file.{Files, Path}
import scala.collection.immutable.VectorMap
import scala.jdk.CollectionConverters._
import scala.language.postfixOps
import scala.util.matching.Regex

package object juice {

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

  val BaseURLRegex: Regex = raw"(http(?:s)?://[a-zA-Z0-9-.]+(?::\d+)?|file://)((?:/[a-zA-Z0-9-.]+)*/?)".r

  case class BaseURL(base: String, path: String)

  def parseurl(s: String): Option[BaseURL] =
    s match {
      case BaseURLRegex(base, path) =>
        Some(
          BaseURL(base,
                  if (path == "") "/"
                  else if (path endsWith "/") path dropRight 1
                  else path))
      case _ => None
    }

  case class Args(verbose: Boolean = false, baseurl: Option[String] = None, cmd: Option[Command] = None)

}
