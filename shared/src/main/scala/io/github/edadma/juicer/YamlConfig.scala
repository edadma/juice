package io.github.edadma.juicer

import io.github.edadma.datetime.Datetime
import io.github.edadma.yaml._
import org.ekrich.config.{Config, ConfigObject, ConfigValueFactory}

import scala.jdk.CollectionConverters._
import scala.language.postfixOps

object YamlConfig {

  def apply(s: String): Config = {
    def construct(n: YamlNode): AnyRef =
      n match {
        case NullYamlNode         => null
        case TimestampYamlNode(t) => Datetime.fromString(t).timestamp
        case MapYamlNode(entries) => (entries map { case (k, v) => (construct(k), construct(v)) } toMap).asJava
        case IntYamlNode(n)       => n.toDouble: Number
        case FloatYamlNode(n)     => n.toDouble: Number
        case StringYamlNode(s)    => s
        case BooleanYamlNode(b)   => (b == "true"): java.lang.Boolean
        case SeqYamlNode(elems)   => elems map construct asJava
      }

    ConfigValueFactory.fromAnyRef(construct(readFromString(s))).asInstanceOf[ConfigObject].toConfig
  }

}
