package io.github.edadma.juice

import java.io.File
import scopt.OParser

import java.nio.file.{Files, Path, Paths}

object Main extends App {

  trait Command

  case class BuildCommand(src: Path = Paths.get("."), dst: Path = null) extends Command

  case class ServeCommand(src: Path = Paths.get("."), dst: Path = null) extends Command

  case object ConfigCommand extends Command

  case class Config(verbose: Boolean = false, baseurl: Option[String] = None, cmd: Option[Command] = None)

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
      note(section("General Options")),
      opt[String]('b', "baseurl")
        .valueName("<URL>")
        .action((b, c) => c.copy(baseurl = Some(b)))
        .text("base site URL"),
      help('h', "help").text("prints this usage text"),
      opt[Unit]('v', "verbose")
        .action((i, c) => c.copy(verbose = true))
        .text("verbose output"),
      version("version").text("prints the version"),
      note(section("Commands")),
      cmd("build")
        .action((_, c) => c.copy(cmd = Some(BuildCommand())))
        .children(
          opt[File]('d', "dest")
            .valueName("<path>")
            .action((o, c) => c.copy(cmd = Some(BuildCommand(dst = o.toPath))))
            .text("output directory path"),
          opt[File]('s', "source")
            .valueName("<path>")
            .action((i, c) => c.copy(cmd = Some(BuildCommand(src = i.toPath))))
            .text("input directory path"),
        ),
      cmd("serve")
        .action((_, c) => c.copy(cmd = Some(ServeCommand())))
        .children(
          opt[File]('d', "dest")
            .valueName("<path>")
            .action((d, c) => c.copy(cmd = Some(ServeCommand(dst = d.toPath))))
            .text("destination directory path"),
          opt[File]('s', "source")
            .valueName("<path>")
            .action((s, c) => c.copy(cmd = Some(ServeCommand(src = s.toPath))))
            .text("source directory path"),
        )
    )
  }

  def isFile(p: Path) = Files.isRegularFile(p) && Files.isReadable(p)

  def isDir(p: Path) = Files.isDirectory(p) && Files.isReadable(p)

  def canCreate(p: Path) = Files.isDirectory(p.getParent) && Files.isWritable(p.getParent)

  def problem(msg: String): Nothing = {
    Console.err.println(msg)
    sys.exit(1)
  }

  OParser.parse(parser, args, Config()) match {
    case Some(Config(_, _, Some(build @ BuildCommand(src, dst)))) =>
      if (!isDir(src)) problem(s"not a readable directory: $src")

      val d = Option(dst) getOrElse (src resolve "public")

      if (!canCreate(d)) problem(s"not a writable directory: $d")

      App(build.copy(dst = d))
    case Some(_) => println(OParser.usage(parser))
    case _       =>
  }

}
