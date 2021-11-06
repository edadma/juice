package io.github.edadma.juicer

import io.github.edadma.datetime.Datetime
import io.github.edadma.yaml._
import org.ekrich.config.Config

object YamlConfig {

  def apply(s: String): Config = {
    def construct(n: YamlNode): Any =
      n match {
        case NullYamlNode         => null
        case TimestampYamlNode(t) => Datetime.fromString(t).timestamp
        case MapYamlNode(entries) => entries map { case (k, v) => (construct(k), construct(v)) } toMap
        case IntYamlNode(n)       => BigDecimal(n)
        case FloatYamlNode(n)     => BigDecimal(n)
        case StringYamlNode(s)    => s
        case BooleanYamlNode(b)   => b == "true"
        case SeqYamlNode(elems)   => elems map construct
      }

    construct(readFromString(s))
  }

}
