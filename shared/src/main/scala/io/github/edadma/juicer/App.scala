package io.github.edadma.juicer

import java.nio.file.{Files, Path, StandardCopyOption}
import scala.collection.immutable.VectorMap
import scala.jdk.CollectionConverters._
import scala.language.postfixOps
import io.github.edadma.cross_platform.readFile
import io.github.edadma.squiggly.{TemplateAST, TemplateBuiltin, TemplateLoader, TemplateParser, TemplateRenderer}
import io.github.edadma.squiggly.platformSpecific.yaml
import io.github.edadma.commonmark.{CommonMarkParser, Util}
import io.github.edadma.squiggly
import org.ekrich.config.{Config, ConfigFactory, ConfigParseOptions, ConfigSyntax, ConfigValueFactory}

import java.io.FileOutputStream
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object App {

  lazy val templateFunctions
    : Map[String, squiggly.TemplateFunction] = TemplateBuiltin.functions ++ JuicerBuiltin.functions
  lazy val templateParser: TemplateParser = new TemplateParser(functions = templateFunctions)
  lazy val markdownParser = new CommonMarkParser

  val run: PartialFunction[Args, Unit] = {
    case Args(verbose, baseurl, Some(BuildCommand(src, dst))) =>
      val src1 = src.normalize.toAbsolutePath
      val dst1 = (Option(dst) getOrElse (src1 resolve "public")).normalize.toAbsolutePath

      if (!isDir(src1)) problem(s"not a readable directory: $src1")
      if (!canCreate(dst1)) problem(s"not a writable directory: $dst1")

      if (!isDir(dst1))
        Files.createDirectory(dst1)

      val siteconf = {
        val c = config(src1, "base")

        baseurl match {
          case None    => c
          case Some(b) => c.withValue("baseURL", ConfigValueFactory.fromAnyRef(b))
        }
      }
      val confdata = configObject(siteconf.root)
      val conf = new ConfigWrapper(siteconf)
      val rendererData = parseurl(conf.baseURL) getOrElse problem(s"invalid base URL: ${conf.baseURL}")
      val site = process(src1, dst1, conf)
      val partialsLoader: TemplateLoader =
        (name: String) =>
          site.partialTemplates find (_.name == name) map (_.template) orElse problem(s"partial '$name' not found")
      val templateRenderer: TemplateRenderer =
        new TemplateRenderer(partials = partialsLoader, functions = templateFunctions, data = rendererData)
      val shortcodesLoader: TemplateLoader =
        (name: String) =>
          site.shortcodeTemplates find (_.name == name) map (_.template) orElse problem(s"shortcode '$name' not found")
      val preprocessor = new Preprocessor(shortcodes = shortcodesLoader, renderer = templateRenderer)

      for (c: ContentFile <- site.content)
        c.content = Util.html(markdownParser.parse(preprocessor.process(c.content)), 2).trim

      val contents = new mutable.LinkedHashMap[String, Any]

      @tailrec
      def put(map: mutable.LinkedHashMap[String, Any], parent: List[String], content: ContentFile): Unit =
        parent match {
          case Nil => map(content.name) = content
          case h :: t =>
            map get h match {
              case Some(m: mutable.LinkedHashMap[String, Any]) => put(m, t, content)
              case Some(_: ContentFile) =>
                problem(s"unexpected content file in place of directory in contents data structure: $h")
              case None =>
                val m = new mutable.LinkedHashMap[String, Any]

                map(h) = m
                put(m, t, content)
            }
        }

      for (page @ ContentFile(outdir, name, data, content) <- site.content) {
        val rel = dst1 relativize outdir

        put(contents, rel.iterator.asScala.toList map (_.toString), page)
      }

      val sitedata = confdata + ("contents" -> contents)

      for (ContentFile(outdir, name, data, content) <- site.content) {
        site.layoutTemplates find (_.name == "page") match {
          case Some(TemplateFile(_, _, template)) =>
            val pagedir = outdir resolve name

            Files.createDirectories(pagedir)

            val out = new FileOutputStream(pagedir resolve "index.html" toString)
            val pagedata = Map("site" -> sitedata, "page" -> data, "content" -> content)

            templateRenderer.render(pagedata, template, out)
            out.close()
          case None => problem(s"'page' layout not found for laying out '$name'")
        }
      }

      for (TemplateFile(path, _, template) <- site.otherTemplates) {
        val out = new FileOutputStream(path.toString)

        templateRenderer.render(Map("site" -> sitedata), template, out)
        out.close()
      }
    case Args(verbose, baseurl, Some(ConfigCommand(src))) =>
      println("Site config:")

      for ((k, v) <- configObject(config(src, "basic").root))
        println(s"  $k = ${renderValue(v)}")
  }

  case class Data(parent: Path, name: String, data: Any)

  case class ContentFile(outdir: Path, name: String, page: Any, var content: String)

  case class TemplateFile(path: Path, name: String, template: TemplateAST)

  case class Site(content: List[ContentFile],
                  data: List[Data],
                  layoutTemplates: List[TemplateFile],
                  partialTemplates: List[TemplateFile],
                  shortcodeTemplates: List[TemplateFile],
                  otherTemplates: List[TemplateFile])

  def process(src: Path, dst: Path, conf: ConfigWrapper): Site = {
    val content = src resolve conf.path.contentDir.normalize
    val layouts = src resolve conf.path.layoutDir.normalize
    val partials = src resolve conf.path.partialDir.normalize
    val shortcodes = src resolve conf.path.shortcodeDir.normalize
    val contentFiles = new ListBuffer[ContentFile]
    val dataFiles = new ListBuffer[Data]
    val layoutTemplates = new ListBuffer[TemplateFile]
    val partialTemplates = new ListBuffer[TemplateFile]
    val shortcodeTemplates = new ListBuffer[TemplateFile]
    val otherTemplates = new ListBuffer[TemplateFile]

    if (!isDir(content)) problem(s"can't read content directory: $content")

    def processDir(dir: Path): Unit = {
      val listing = list(dir)

      excludeDirs(listing, dst) foreach processDir

      if (dir startsWith content)
        includeExts(listing, "MD", "md", "markdown") foreach { p =>
          val s = readFile(p.toString)
          val lines = scala.io.Source.fromString(s).getLines()
          val (first, data) = {
            val first = lines.next()

            first match {
              case "---" =>
                val buf = new StringBuilder

                @tailrec
                def line(): Unit =
                  if (lines.hasNext) {
                    lines.next() match {
                      case "---" =>
                      case s =>
                        buf ++= s
                        buf += '\n'
                        line()
                    }
                  } else
                    problem(s"unexpected end of file while reading front matter: $p")

                line()
                (first, buf.toString)
              case _ => (first, "")
            }
          }

          val outdir = dst resolve (content relativize p.getParent)

          contentFiles += ContentFile(outdir,
                                      withoutExtension(p.getFileName.toString),
                                      yaml(data),
                                      ((if (first == "---") ""
                                        else first :+ '\n') ++ (lines map (_ :+ '\n') mkString)).trim)
        }

      includeExts(listing, "YML", "YAML", "yml", "yaml") foreach (p =>
        dataFiles += Data(p.getParent, withoutExtension(p.getFileName.toString), yaml(readFile(p.toString))))

      if (dir startsWith layouts)
        includeExts(listing, "html", "sq") foreach { p =>
          layoutTemplates += TemplateFile(p.getParent,
                                          withoutExtension(p.getFileName.toString),
                                          templateParser.parse(readFile(p.toString)))
        }

      if (dir startsWith partials)
        includeExts(listing, "html", "sq") foreach { p =>
          partialTemplates += TemplateFile(p.getParent,
                                           withoutExtension(p.getFileName.toString),
                                           templateParser.parse(readFile(p.toString)))
        }

      if (dir startsWith shortcodes)
        includeExts(listing, "html", "sq") foreach { p =>
          shortcodeTemplates += TemplateFile(p.getParent,
                                             withoutExtension(p.getFileName.toString),
                                             templateParser.parse(readFile(p.toString)))
        }

      if ((src == layouts || !dir.startsWith(layouts)) && (src == partials || !dir.startsWith(partials)) && (src == shortcodes || !dir
            .startsWith(shortcodes)))
        includeExts(listing, "html", "css", "scss", "sass") foreach { p =>
          val outfile = dst resolve (src relativize p)

          otherTemplates += TemplateFile(outfile, null, templateParser.parse(readFile(p.toString)))
        }

      Files.createDirectories(dst resolve (src relativize dir))
      excludeExts(listing,
                  "html",
                  "sq",
                  "css",
                  "scss",
                  "sass",
                  "YML",
                  "YAML",
                  "yml",
                  "yaml",
                  "MD",
                  "md",
                  "markdown",
                  "props",
                  "properties",
                  "conf",
                  "hocon") foreach { p =>
        val target = dst resolve (src relativize p)

        Files.copy(p, target, StandardCopyOption.REPLACE_EXISTING)
      }
    }

    processDir(src)
    Site(contentFiles.toList,
         dataFiles.toList,
         layoutTemplates.toList,
         partialTemplates.toList,
         shortcodeTemplates.toList,
         otherTemplates.toList)
  }

  def renderValue(v: Any): String =
    v match {
      case s: String          => s"${'"'}$s${'"'}"
      case n: Int             => n.toString
      case n: Double          => n.toString
      case b: Boolean         => b.toString
      case l: List[_]         => l map renderValue mkString ("[", ", ", "]")
      case m: VectorMap[_, _] => m map { case (k, v) => s"$k: ${renderValue(v)}" } mkString ("{", ", ", "}")
    }

  def list(dir: Path): List[Path] = Files.list(dir).iterator.asScala.toList

  def includeExts(listing: List[Path], exts: String*): List[Path] = {
    val suffixes = exts map ('.' +: _)

    listing filter (p => isFile(p) && (suffixes.isEmpty || suffixes.exists(p.getFileName.toString endsWith _))) sortBy (_.getFileName.toString)
  }

  def excludeExts(listing: List[Path], exts: String*): List[Path] = {
    require(exts.nonEmpty)

    val suffixes = exts map ('.' +: _)

    listing filter (p => isFile(p) && (!suffixes.exists(p.getFileName.toString endsWith _)))
  }

  def excludeDirs(listing: List[Path], exclude: Path*): List[Path] =
    listing filter (p => isDir(p) && !exclude.contains(p))

  def extension(filename: String): String =
    filename lastIndexOf '.' match {
      case -1  => ""
      case dot => filename substring (dot + 1)
    }

  def withoutExtension(filename: String): String =
    filename lastIndexOf '.' match {
      case -1  => filename
      case dot => filename substring (0, dot)
    }

  def readConfig(path: Path): Config = {
    val file = path.toString
    val syntax =
      extension(file) match {
        case "json"                 => ConfigParseOptions.defaults.setSyntax(ConfigSyntax.JSON)
        case "conf" | "hocon"       => ConfigParseOptions.defaults.setSyntax(ConfigSyntax.CONF)
        case "props" | "properties" => ConfigParseOptions.defaults.setSyntax(ConfigSyntax.PROPERTIES)
      }

    ConfigFactory.parseString(readFile(file), syntax)
  }

  def config(src: Path, base: String): Config = {
    BaseConfig(base) match {
      case Some(b) =>
        includeExts(list(src), "json", "conf", "properties", "props", "hocon").foldLeft(b) {
          case (c, p) => readConfig(p) withFallback c
        }
      case None => problem(s"unknown base configuration: $base")
    }
  }

}
