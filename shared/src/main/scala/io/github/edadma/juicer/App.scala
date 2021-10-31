package io.github.edadma.juicer

import java.nio.file.{Files, Path}
import scala.collection.immutable.VectorMap
import scala.jdk.CollectionConverters._
import scala.language.postfixOps
import io.github.edadma.cross_platform.readFile
import io.github.edadma.squiggly.{TemplateLoader, TemplateRenderer}
import io.github.edadma.commonmark.Util
import org.ekrich.config.{Config, ConfigFactory, ConfigParseOptions, ConfigSyntax, ConfigValueFactory}

import java.io.FileOutputStream
import scala.annotation.tailrec
import scala.collection.mutable

object App {

  val run: PartialFunction[Args, Unit] = {
    case Args(baseConfig, verbose, baseurl, Some(BuildCommand(src, dst))) =>
      build(baseConfig, verbose, baseurl, src, dst)
    case Args(baseConfig, _, baseurl, Some(ConfigCommand(src))) =>
      println("Site config:")

      val c = config(src, baseConfig)
      val c1 =
        baseurl match {
          case None    => c
          case Some(b) => c.withValue("baseURL", ConfigValueFactory.fromAnyRef(b))
        }

      for ((k, v) <- configObject(c1.root))
        println(s"  $k = ${renderValue(v)}")
  }

  def build(baseConfig: String, verbose: Boolean, baseurl: Option[String], src: Path, dst: Path): Unit = {
    showSteps = verbose

    val src1 = src.normalize.toAbsolutePath

    show(s"source path = $src1")

    if (!isDir(src1)) problem(s"not a readable directory: $src1")

    val (dst1, siteconf) = {
      val c = config(src1, baseConfig)
      val c1 =
        baseurl match {
          case None    => c
          case Some(b) => c.withValue("baseURL", ConfigValueFactory.fromAnyRef(b))
        }
      val (d, c2) =
        dst match {
          case null =>
            val p = (src1 resolve "public").normalize.toAbsolutePath

            (p, c1.withValue("publicDir", ConfigValueFactory.fromAnyRef(p.toString)))
          case d =>
            val p = d.normalize.toAbsolutePath

            (p, c1.withValue("publicDir", ConfigValueFactory.fromAnyRef(p.toString)))
        }

      (d, c2)
    }

    show(s"destination path = $dst1")

    if (!canCreate(dst1)) problem(s"not a writable directory: $dst1")

    if (!isDir(dst1)) {
      show(s"create destination path $dst1")
      Files.createDirectory(dst1)
    }

    val confdata = configObject(siteconf.root)
    val conf = new ConfigWrapper(siteconf)
    val rendererData = parseurl(conf.baseURL) getOrElse problem(s"invalid base URL: ${conf.baseURL}")

    show(s"base URL = ${rendererData.base}${rendererData.path}")

    val site = Process(src1, dst1, conf)
    val partialsLoader: TemplateLoader =
      (name: String) =>
        site.partialTemplates find (_.name == name) map (_.template) orElse problem(s"partial '$name' not found")
    val templateRenderer: TemplateRenderer =
      new TemplateRenderer(partials = partialsLoader, functions = templateFunctions, data = rendererData)
    val shortcodesLoader: TemplateLoader =
      (name: String) =>
        site.shortcodeTemplates find (_.name == name) map (_.template) orElse problem(s"shortcode '$name' not found")
    val preprocessor = new Preprocessor(shortcodes = shortcodesLoader, renderer = templateRenderer)

    for (c @ ContentFile(_, name, _, _, _, _) <- site.content) {
      show(s"parse markdown file $name")
      val doc = markdownParser.parse(preprocessor.process(c.source))

      c.content = Util.html(doc, 2).trim
      c.toc = Util.toc(doc)
    }

    val contents = new mutable.LinkedHashMap[String, Any]

    @tailrec
    def put(map: mutable.LinkedHashMap[String, Any], parent: List[String], content: ContentFile): Unit =
      parent match {
        case Nil => map(content.name) = content
        case h :: t =>
          map get h match {
            case Some(m: mutable.LinkedHashMap[_, _]) =>
              put(m.asInstanceOf[mutable.LinkedHashMap[String, Any]], t, content)
            case Some(_: ContentFile) =>
              problem(s"unexpected content file in place of directory in contents data structure: $h")
            case Some(_) => problem("problem")
            case None =>
              val m = new mutable.LinkedHashMap[String, Any]

              map(h) = m
              put(m, t, content)
          }
      }

    site.content foreach {
      case page @ ContentFile(outdir, name, _, _, _, _) =>
        val rel = dst1 relativize outdir

        put(contents, rel.iterator.asScala.toList map (_.toString), page)
      case _: ContentFolder =>
    }

    val sitedata = confdata + ("contents" -> contents)

    for (ContentFile(outdir, name, data, _, content, toc) <- site.content) {
      site.layoutTemplates find (_.name == "default") match {
        case Some(TemplateFile(templatePath, templateName, template)) =>
          show(s"render $name using ${src1 relativize templatePath resolve templateName}")

          if (name == "index") {
            val outfile = outdir resolve "index.html" toString

            show(s"content: write file $outfile")

            val out = new FileOutputStream(outfile)
            val pagedata = Map("site" -> sitedata, "page" -> data, "content" -> content, "toc" -> toc)

            templateRenderer.render(pagedata, template, out)
            out.close()
          } else {
            val pagedir = outdir resolve name

            show(s"content: create directory $pagedir")
            Files.createDirectories(pagedir)

            val outfile = pagedir resolve "index.html" toString

            show(s"content: write file $outfile")

            val out = new FileOutputStream(outfile)
            val pagedata = Map("site" -> sitedata, "page" -> data, "content" -> content, "toc" -> toc)

            templateRenderer.render(pagedata, template, out)
            out.close()
          }
        case None => problem(s"'default' layout not found for laying out '$name'")
      }
    }

    for (TemplateFile(path, _, template) <- site.otherTemplates) {
      show(s"template: write file $path")

      val out = new FileOutputStream(path.toString)

      templateRenderer.render(Map("site" -> sitedata), template, out)
      out.close()
    }
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

  def extension(filename: String): String =
    filename lastIndexOf '.' match {
      case -1  => ""
      case dot => filename substring (dot + 1)
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
    BaseConfigs(base) match {
      case Some(b) =>
        filesIncludingExtensions(list(src), "json", "conf", "properties", "props", "hocon").foldLeft(b) {
          case (c, p) => readConfig(p) withFallback c
        }
      case None => problem(s"unknown base configuration: $base")
    }
  }

}
