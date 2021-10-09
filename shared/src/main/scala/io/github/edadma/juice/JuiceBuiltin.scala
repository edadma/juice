package io.github.edadma.juice

import io.github.edadma.squiggly.TemplateFunction

import java.nio.file.Paths
import scala.language.postfixOps

object JuiceBuiltin {

  val functions: Map[String, TemplateFunction] =
    List(
      TemplateFunction("absURL", 1, {
        case (con, Seq(arg: String)) =>
          val BaseURL(base, path) = con.renderer.data.asInstanceOf[BaseURL]

          s"$base${Paths.get(path) resolve arg}"
      }),
      TemplateFunction("juiceurl", 0, _ => "https://github.com/edadma/juice"),
      TemplateFunction("relURL", 1, {
        case (con, Seq(arg: String)) => Paths.get(con.renderer.data.asInstanceOf[BaseURL].path) resolve arg toString
      }),
    ) map (f => (f.name, f)) toMap

}
