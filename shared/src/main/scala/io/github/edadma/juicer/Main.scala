package io.github.edadma.juicer

import java.io.File
import scopt.OParser

object Main extends App {

  val builder = OParser.builder[Args]
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
      programName("juicer"),
      head("Juicer Site Generator", "v0.1.0"),
      note(section("General Options")),
      opt[String]('b', "baseurl")
        .valueName("<URL>")
        .action((b, c) => c.copy(baseurl = Some(b)))
        .text("base site URL"),
      opt[String]('c', "config")
        .valueName("<URL>")
        .action((b, c) => c.copy(config = b))
        .text("base site configuration (default is 'standard')"),
      help('h', "help").text("prints this usage text"),
      opt[Unit]('v', "verbose")
        .action((_, c) => c.copy(verbose = true))
        .text("verbose output"),
      version("version").text("prints the version"),
      note(section("Commands")),
      cmd("build")
        .action((_, c) => c.copy(cmd = Some(BuildCommand())))
        .text("  Build the site")
        .children(
          opt[File]('d', "dest")
            .valueName("<path>")
            .action((o, c) => c.copy(cmd = Some(BuildCommand(dst = o.toPath))))
            .text("destination directory path"),
          opt[File]('s', "source")
            .valueName("<path>")
            .action((i, c) => c.copy(cmd = Some(BuildCommand(src = i.toPath))))
            .text("site sources directory path"),
        ),
      cmd("config")
        .action((_, c) => c.copy(cmd = Some(ConfigCommand())))
        .text("  Show build configuration")
        .children(
          opt[File]('s', "source")
            .valueName("<path>")
            .action((s, c) => c.copy(cmd = Some(ConfigCommand(src = s.toPath))))
            .text("site sources directory path")),
      cmd("serve")
        .action((_, c) => c.copy(cmd = Some(ServeCommand())))
        .text("  Build and serve the site")
        .children(
          opt[File]('d', "dest")
            .valueName("<path>")
            .action((d, c) => c.copy(cmd = Some(ServeCommand(dst = d.toPath))))
            .text("destination directory path"),
          opt[File]('s', "source")
            .valueName("<path>")
            .action((s, c) => c.copy(cmd = Some(ServeCommand(src = s.toPath))))
            .text("site sources directory path"),
        )
    )
  }

  OParser.parse(parser, args, Args()) match {
    case Some(args: Args) if args.cmd.nonEmpty => App run args
    case Some(_)                               => println(OParser.usage(parser))
    case _                                     =>
  }

}
