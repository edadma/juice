package io.github.edadma.juicer

import org.ekrich.config.{Config, ConfigList}
import org.ekrich.config.impl.ConfigString

import java.nio.file.{Path, Paths}
import scala.language.dynamics

class ConfigWrapper(c: Config) extends Dynamic {

  object IntDynamic extends Dynamic {
    def selectDynamic(name: String): Int = c.getInt(name)
  }

  object DoubleDynamic extends Dynamic {
    def selectDynamic(name: String): Double =
      c.getNumber(name) match {
        case n: java.lang.Integer => n.toDouble
        case n: java.lang.Double  => n
      }
  }

  object BooleanDynamic extends Dynamic {
    def selectDynamic(name: String): Boolean = c.getBoolean(name)
  }

  object PathDynamic extends Dynamic {
    def selectDynamic(name: String): Path = Paths.get(c.getString(name))
  }

  object PathsDynamic extends Dynamic {
    def selectDynamic(name: String): List[Path] = stringList(name) map (Paths.get(_))
  }

  private def stringList(name: String) =
    c.getValue(name) match {
      case s: ConfigString => List(s.unwrapped)
      case l: ConfigList   => configList(l) map (_.toString)
    }

  object ListDynamic extends Dynamic {
    def selectDynamic(name: String): List[String] = stringList(name)
  }

  def int: IntDynamic.type = IntDynamic

  def list: ListDynamic.type = ListDynamic

  def double: DoubleDynamic.type = DoubleDynamic

  def boolean: BooleanDynamic.type = BooleanDynamic

  def path: PathDynamic.type = PathDynamic

  def paths: PathsDynamic.type = PathsDynamic

  def selectDynamic(name: String): String = c.getString(name)

}
