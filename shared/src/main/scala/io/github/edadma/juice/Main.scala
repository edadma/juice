package io.github.edadma.juice

import java.io.File
import scopt.OParser

object Main extends App {

  case class Config(cmd: Option[String] = None)

  val builder = OParser.builder[Config]
  val parser = {
    import builder._

    val BOLD = Console.BOLD
    var firstSection = true

    def section(name: String) = {
      val res =
        s"${if (!firstSection) "\n" else ""}$BOLD\u2501\u2501\u2501\u2501\u2501 $name ${"\u2501" * (20 - name.length)}${Console.RESET}"

      firstSection = false
      res
    }

    OParser.sequence(
      programName("juice"),
      head("Juice Site Generator", "v0.1.0"),
      note(section("Options")),
      help('h', "help").text("prints this usage text"),
      version('v', "version").text("prints the version"),
      note(section("Commands")),
      cmd("build")
        .action((_, c) => c.copy(cmd = Some("build")))
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(Config(None)) => println(OParser.usage(parser))
    case Some(conf)         => app(conf)
    case _                  =>
  }

  def app(c: Config): Unit = {

    //

  }

}
