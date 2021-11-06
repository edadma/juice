package io.github.edadma.juicer

import io.github.edadma.commonmark.TOC
import io.github.edadma.cross_platform.readFile
import io.github.edadma.squiggly.TemplateAST
import io.github.edadma.squiggly.platformSpecific.yaml

import java.io.File
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.language.postfixOps

object Process {

  def apply(src: Path, dst: Path, conf: ConfigWrapper): Site = {
    val content = src resolve conf.path.contentDir.normalize
    val html = conf.htmlDir
    val stripPrefix = conf.boolean.stripPrefix
    val static = src resolve conf.path.staticDir.normalize
    val layouts = src resolve conf.path.layoutDir.normalize
    val partials = src resolve conf.path.partialDir.normalize
    val shortcodes = src resolve conf.path.shortcodeDir.normalize
    val folderContent = conf.folderContent
    val contentItems = new ListBuffer[ContentItem]
    val contentMap = new mutable.HashMap[String, ContentFile]
    val dataFiles = new ListBuffer[DataFile]
    val layoutTemplates = new mutable.HashMap[(List[String], String), TemplateFile]
    val partialTemplates = new mutable.HashMap[String, TemplateFile]
    val shortcodeTemplates = new mutable.HashMap[String, TemplateFile]
    val otherTemplates = new ListBuffer[TemplateFile]

    if (!isDir(content)) problem(s"can't read content directory: $content")

    def processDir(dir: Path): Unit = {
      show(s">>> $dir")

      val listing = list(dir)

      if (dir startsWith content) {
        val files = filesIncludingExtensions(listing, "md", "markdown", "mkd", "mkdn", "mdown")
        val outdir = {
          val uncleaned = dst resolve (content relativize dir)

          if (contentItems.isEmpty) {
            if (uncleaned == dst) dst
            else
              (if (html == "") dst else dst resolve html) resolve
                clean(uncleaned.getFileName.toString, stripPrefix = true)
          } else {
            val prev = contentItems.last.outdir

            if (prev.getNameCount >= uncleaned.getNameCount)
              Paths.get(File.separator) resolve prev.subpath(0, uncleaned.getNameCount - (if (html == "") 1 else 0)) resolve
                clean(uncleaned.getFileName.toString, stripPrefix = true)
            else
              (if (html == "") prev else prev resolve html) resolve
                clean(uncleaned.getFileName.toString, stripPrefix = true)
          }
        }

        if (outdir != dst) {
          show(s"content destination subfolder: $outdir")
          contentItems += ContentFolder(outdir)
        }

        show(s"content file(s): ${files map (_.getFileName) mkString ", "}", files.nonEmpty)
        show("no content files", files.isEmpty)

        files foreach { p =>
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

          val name =
            withoutExtension(p.getFileName.toString) match {
              case `folderContent` => folderContent
              case n               => clean(n, stripPrefix)
            }
          val contentFile = ContentFile(
            outdir,
            name,
            yaml(data),
            ((if (first == "---") ""
              else first :+ '\n') ++ (lines map (_ :+ '\n') mkString)).trim,
            null,
            null
          )

          contentMap(content relativize p toString) = contentFile
          contentItems += contentFile
        }
      }

      val data =
        filesIncludingExtensions(listing, "YML", "YAML", "yml", "yaml")

      show(s"data files: ${data map (_.getFileName) mkString ", "}", data.nonEmpty)
      data foreach (p =>
        dataFiles += DataFile(dir, withoutExtension(p.getFileName.toString), yaml(readFile(p.toString))))

      if (dir startsWith layouts) {
        val folder = (layouts relativize dir).iterator.asScala.toList map (_.toString)
        val files = filesIncludingExtensions(listing, "html", "sq")

        show(s"layouts: ${files map (_.getFileName) mkString ", "}", files.nonEmpty)

        files foreach { p =>
          val name = withoutExtension(p.getFileName.toString)

          layoutTemplates((folder, name)) = TemplateFile(p, name, null)
        }
      }

      if (dir startsWith partials) {
        val files = filesIncludingExtensions(listing, "html", "sq")

        show(s"partials: ${files map (_.getFileName) mkString ", "}", files.nonEmpty)

        files foreach { p =>
          val name = withoutExtension(p.getFileName.toString)

          partialTemplates(name) = TemplateFile(p, name, null)
        }
      }

      if (dir startsWith shortcodes) {
        val files = filesIncludingExtensions(listing, "html", "sq")

        show(s"shortcodes: ${files map (_.getFileName) mkString ", "}", files.nonEmpty)

        files foreach { p =>
          val name = withoutExtension(p.getFileName.toString)

          shortcodeTemplates(name) = TemplateFile(p, name, null)
        }
      }

      if (dir startsWith static) {
        val subdir = dst resolve (static relativize dir)

        show(s"static: create directory $subdir")
        Files.createDirectories(subdir)

        (if (static == src)
           filesExcludingExtensions(listing,
                                    "html",
                                    "sq",
                                    "css",
                                    "scss",
                                    "sass",
                                    "YML",
                                    "YAML",
                                    "yml",
                                    "yaml",
                                    "mkd",
                                    "mkdn",
                                    "mdown",
                                    "md",
                                    "markdown",
                                    "props",
                                    "properties",
                                    "conf",
                                    "hocon")
         else listing filter isFile) foreach { p =>
          val dp = dst resolve (static relativize p)

          show(s"static: copy $p => $dp")
          Files.copy(p, dp, StandardCopyOption.REPLACE_EXISTING)
        }
      }

      if (!(layouts != src && dir.startsWith(layouts)) &&
          !(partials != src && dir.startsWith(partials)) &&
          !(shortcodes != src && dir.startsWith(shortcodes)) &&
          !(static != src && dir.startsWith(static))) {
        val l = filesIncludingExtensions(listing, "html", "css", "scss", "sass")

        show(s"other templates: ${l map (_.getFileName) mkString ", "}", l.nonEmpty)
        l foreach { p =>
          val outfile = dst resolve (src relativize p)

          show(s"parse template $p")
          otherTemplates += TemplateFile(outfile, null, templateParser.parse(readFile(p.toString)))
        }
      }

      dirsExcluding(listing, dst) foreach processDir
    }

    processDir(src)
    Site(contentItems.toList,
         contentMap.toMap,
         dataFiles.toList,
         layoutTemplates.toMap,
         partialTemplates.toMap,
         shortcodeTemplates.toMap,
         otherTemplates.toList)
  }

  def withoutExtension(filename: String): String =
    filename lastIndexOf '.' match {
      case -1  => filename
      case dot => filename substring (0, dot)
    }

  def clean(s: String, stripPrefix: Boolean): String = {
    val buf = new StringBuilder(s)

    if (stripPrefix) {
      while (buf.nonEmpty && buf.head.isDigit) buf.deleteCharAt(0)
      while (buf.nonEmpty && !buf.head.isLetterOrDigit) buf.deleteCharAt(0)
    }

    @tailrec
    def clean(from: Int): Unit =
      buf.indexWhere(!_.isLetterOrDigit, from) match {
        case -1 =>
        case idx =>
          buf.indexWhere(_.isLetterOrDigit, idx) match {
            case -1 =>
              buf.delete(idx, buf.length)
            case end =>
              buf(idx) = '-'
              buf.delete(idx + 1, end)
              clean(idx + 1)
          }
      }

    clean(0)

    if (buf.isEmpty) "-"
    else buf.toString
  }

}

case class DataFile(parent: Path, name: String, data: Any)

trait ContentItem { val outdir: Path }
case class ContentFile(outdir: Path, name: String, page: Any, source: String, var content: String, var toc: TOC)
    extends ContentItem
case class ContentFolder(outdir: Path) extends ContentItem

case class TemplateFile(path: Path, name: String, var template: TemplateAST)

case class Site(content: List[ContentItem],
                map: Map[String, ContentFile],
                data: List[DataFile],
                layoutTemplates: Map[(List[String], String), TemplateFile],
                partialTemplates: Map[String, TemplateFile],
                shortcodeTemplates: Map[String, TemplateFile],
                otherTemplates: List[TemplateFile])
