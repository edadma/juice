package io.github.edadma.juice

import org.ekrich.config.{Config, ConfigList}
import org.ekrich.config.impl.ConfigString

import scala.language.dynamics

class ConfigWrapper(c: Config) extends Dynamic {

  object IntDynamic extends Dynamic {
    def selectDynamic(name: String): Int = c.getInt(name)
  }

  object DoubleDynamic extends Dynamic {
    def selectDynamic(name: String): Double =
      c.getNumber(name) match {
        case n: java.lang.Integer => n.toDouble
        case n: java.lang.Double => n
      }
  }

  object BooleanDynamic extends Dynamic {
    def selectDynamic(name: String): Boolean = c.getBoolean(name)
  }

  object ListDynamic extends Dynamic {
    def selectDynamic(name: String): List[String] =
      c.getValue(name) match {
        case s: ConfigString => List(s.unwrapped)
        case l: ConfigList => configList(l) map (_.toString)
      }
  }

  def apply(path: String): String = c.getString(path)

  def int: IntDynamic.type = IntDynamic

  def list: ListDynamic.type = ListDynamic

  def double: DoubleDynamic.type = DoubleDynamic

  def boolean: BooleanDynamic.type = BooleanDynamic

}