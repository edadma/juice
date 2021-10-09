package io.github.edadma.juice

import io.github.edadma.squiggly.TemplateFunction

import scala.language.postfixOps

object JuiceBuiltin {

  val functions: Map[String, TemplateFunction] =
    List(
      TemplateFunction("juiceurl", 0, _ => "https://github.com/edadma/juice")
    ) map (f => (f.name, f)) toMap

}
