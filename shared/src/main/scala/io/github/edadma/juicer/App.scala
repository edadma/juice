package io.github.edadma.juicer

import java.nio.file.{Files, Path}
import scala.collection.immutable.VectorMap
import scala.jdk.CollectionConverters._
import scala.language.postfixOps
import io.github.edadma.cross_platform.readFile
import io.github.edadma.squiggly.{TemplateAST, TemplateLoader, TemplateRenderer}
import io.github.edadma.commonmark
import io.github.edadma.commonmark.Heading
import org.ekrich.config.{Config, ConfigFactory, ConfigParseOptions, ConfigSyntax, ConfigValueFactory}

import java.io.{FileOutputStream, OutputStream}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

      dst match {
        case null =>
          val p = (src1 resolve "public").normalize.toAbsolutePath

          (p, c1.withValue("publicDir", ConfigValueFactory.fromAnyRef(p.toString)))
        case d =>
          val p = d.normalize.toAbsolutePath

          (p, c1.withValue("publicDir", ConfigValueFactory.fromAnyRef(p.toString)))
      }
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
        site.partialTemplates get name map { t =>
          if (t.template eq null)
            t.template = templateParser.parse(readFile(t.path.toString))

          t.template
        } orElse problem(s"partial '$name' not found")
    val templateRenderer: TemplateRenderer =
      new TemplateRenderer(partials = partialsLoader, functions = templateFunctions, data = rendererData)
    val shortcodesLoader: TemplateLoader =
      (name: String) =>
        site.shortcodeTemplates get name map { t =>
          if (t.template eq null)
            t.template = templateParser.parse(readFile(t.path.toString))

          t.template
        } orElse problem(s"shortcode '$name' not found")
    val preprocessor = new Preprocessor(shortcodes = shortcodesLoader, renderer = templateRenderer)

    for (c @ ContentFile(_, name, _, _, _, _) <- site.content) {
      show(s"parse markdown file $name")

      val doc = markdownParser.parse(preprocessor.process(c.source))

      c.toc = commonmark.Util.toc(doc)
      c.content = commonmark.Util.html(doc, 2).trim
    }

//    @tailrec
//    def put(map: mutable.LinkedHashMap[String, Any], parent: List[String], content: ContentFile): Unit =
//      parent match {
//        case Nil => map(content.name) = content
//        case h :: t =>
//          map get h match {
//            case Some(m: mutable.LinkedHashMap[_, _]) =>
//              put(m.asInstanceOf[mutable.LinkedHashMap[String, Any]], t, content)
//            case Some(_: ContentFile) =>
//              problem(s"unexpected content file in place of directory in contents data structure: $h")
//            case Some(_) => problem("problem")
//            case None =>
//              val m = new mutable.LinkedHashMap[String, Any]
//
//              map(h) = m
//              put(m, t, content)
//          }
//      }
//
//    val contents = new mutable.LinkedHashMap[String, Any]

    trait TOCItem
    case class TOCLabel(label: String) extends TOCItem
    case class TOCLink(html: String, href: String)
    case class TOCList(headings: List[TOCLink]) extends TOCItem

    val sitetoc = new ListBuffer[TOCItem]

    //    val html = conf.htmlDir
//
//    site.content foreach {
//      case page @ ContentFile(outdir, _, _, _, _, toc) =>
////        val rel = (if (html == "") dst1 else dst1 resolve html) relativize outdir
////
////        put(contents, rel.iterator.asScala.toList map (_.toString), page)
//        sitetoc += TOCItem("file", commonmark.Util.html(toc.headings.head.heading.contents, 2).trim)
//      case ContentFolder(outdir) => sitetoc += TOCItem("folder", outdir.getFileName.toString)
//    }
//
//    val sitedata = confdata + /*("contents" -> contents) + */ ("toc" -> sitetoc)

    @tailrec
    def mktocFromContent(l: List[ContentItem], start: String = null): String =
      l match {
        case Nil => start
        case ContentLabel(label) :: t =>
          sitetoc += TOCLabel(label)
          mktocFromContent(t, start)
        case ContentFolder(outdir) :: t =>
          sitetoc += TOCLabel(outdir.getFileName.toString)
          mktocFromContent(t, start)
        case (c: ContentFile) :: _ =>
          val (headings: List[ContentFile], rest) = l span (_.isInstanceOf[ContentFile])

          sitetoc += TOCList(
            headings map (
                h =>
                  TOCLink(
                    commonmark.Util.html(h.toc.headings.head.heading.contents, 2).trim,
                    s"${dst1 relativize h.outdir}/${h.name}"
                  )))
          mktocFromContent(rest, if (start eq null) s"${dst1 relativize c.outdir}/${c.name}/" else start)
      }

    def mktocFromConfig: String = {
      val buf = new ListBuffer[ContentItem]

      confdata("nav") match {
        case l: List[_] =>
          l foreach {
            case label: String =>
              if (markdownExtensions exists (label endsWith _))
                buf += site.map.getOrElse(label, problem(s"content file not found $label"))
              else
                buf += ContentLabel(label)
            case file: Map[_, _]
                if file.size == 1 && file.head._1.isInstanceOf[String] && file.head._2.isInstanceOf[String] =>
              val (name: String, path: String) = file.head

              buf += site.map.getOrElse(path, problem(s"content file not found $path"))
            case e => problem(s"invalid nav element: $e")
          }

          mktocFromContent(buf.toList)
        case n => problem(s"invalid 'nav': $n")
      }
    }

    val start =
      if (confdata contains "nav") mktocFromConfig
      else if (site.content.nonEmpty) mktocFromContent(site.content.tail)
    val sitedata = confdata + ("toc" -> sitetoc.toList) + ("start" -> start)
    val defaultLayout = conf.defaultLayout
    val baseofLayout = conf.baseofLayout
    val fileLayout = conf.fileLayout
    val folderLayout = conf.folderLayout
    val folderContent = conf.folderContent
    val html = conf.htmlDir

    def findLayout(folders: List[String], name: String): Option[TemplateFile] =
      site.layoutTemplates get (folders, name) orElse (if (folders.isEmpty)
                                                         site.layoutTemplates get (List(defaultLayout), name)
                                                       else findLayout(folders.init, name)) map { t =>
        if (t.template eq null)
          t.template = templateParser.parse(readFile(t.path.toString))

        t
      }

    case class SubHeading(heading: String, id: String, sub: List[SubHeading])

    def subheadings(l: List[Heading]): List[SubHeading] =
      l map (h =>
        SubHeading(commonmark.Util.html(h.heading.contents, 2).trim, h.heading.id.get, subheadings(h.sub.headings)))

    for (ContentFile(outdir, name, data, _, content, toc) <- site.content) {
      templateRenderer.blocks.clear()

      val outfile =
        if (name == folderContent) outdir resolve "index.html" toString
        else {
          val pagedir = outdir resolve name

          show(s"content: create directory $pagedir")
          Files.createDirectories(pagedir)
          pagedir resolve "index.html" toString
        }
      val sub = {
        toc.headings.headOption match {
          case Some(h) => subheadings(h.sub.headings)
          case None    => Nil
        }
      }
      val pagedata =
        Map("site" -> sitedata, "page" -> data, "content" -> content, "toc" -> toc, "sub" -> sub)
      val folders = {
        val rel = dst1 relativize outdir

        if (dst1 == outdir) Nil
        else (if (html != "") rel.subpath(1, rel.getNameCount) else rel).iterator.asScala.toList map (_.toString)
      }
      val layout = if (name == folderContent) folderLayout else fileLayout
      val particularTemplate =
        findLayout(folders, layout) match {
          case Some(TemplateFile(templatePath, templateName, template)) =>
            show(s"render $name using ${src1 relativize templatePath}")
            Some(template)
          case None =>
            show(s"layout '$layout' not found for rendering '$name'")
            None
        }
      val baseofTemplate =
        findLayout(folders, baseofLayout) match {
          case Some(TemplateFile(templatePath, templateName, template)) =>
            show(s"render $name using ${src1 relativize templatePath}")
            Some(template)
          case None =>
            show(s"layout '$baseofLayout' not found for rendering '$name'")
            None
        }

      def render(template: TemplateAST): Unit = {
        show(s"content: write file $outfile")

        val out = new FileOutputStream(outfile)

        templateRenderer.render(pagedata, template, out)
        out.close()
      }

      (particularTemplate, baseofTemplate) match {
        case (None, None) => problem(s"no template was found for rendering $name")
        case (Some(p), Some(b)) =>
          templateRenderer.render(pagedata, p, OutputStream.nullOutputStream)
          render(b)
        case (Some(p), None) => render(p)
        case (None, Some(b)) => render(b)
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
    val ext = extension(file)
    val conf = readFile(file)

    ext match {
      case "yaml" | "yml" => YamlConfig(conf)
      case _ =>
        val syntax =
          ext match {
            case "json"                 => ConfigParseOptions.defaults.setSyntax(ConfigSyntax.JSON)
            case "conf" | "hocon"       => ConfigParseOptions.defaults.setSyntax(ConfigSyntax.CONF)
            case "props" | "properties" => ConfigParseOptions.defaults.setSyntax(ConfigSyntax.PROPERTIES)
          }

        ConfigFactory.parseString(conf, syntax)
    }
  }

  def config(src: Path, base: String): Config = {
    BaseConfigs(base) match {
      case Some(b) =>
        filesIncludingExtensions(list(src), "json", "conf", "properties", "props", "hocon", "yaml", "yml").foldLeft(b) {
          case (c, p) => readConfig(p) withFallback c
        }
      case None => problem(s"unknown base configuration: $base")
    }
  }

}
