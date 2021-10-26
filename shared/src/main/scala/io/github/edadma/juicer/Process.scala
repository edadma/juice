package io.github.edadma.juicer

import io.github.edadma.commonmark.TOC
import io.github.edadma.cross_platform.readFile
import io.github.edadma.squiggly.TemplateAST
import io.github.edadma.squiggly.platformSpecific.yaml

import java.nio.file.{Files, Path, StandardCopyOption}
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

object Process {

  def apply(src: Path, dst: Path, conf: ConfigWrapper): Site = {
    val content = src resolve conf.path.contentDir.normalize
    val static = src resolve conf.path.staticDir.normalize
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
                                        else first :+ '\n') ++ (lines map (_ :+ '\n') mkString)).trim,
                                      null,
                                      null)
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

      if ((src == layouts || !dir.startsWith(layouts)) && (src == partials || !dir.startsWith(partials)) &&
          (src == shortcodes || !dir.startsWith(shortcodes)))
        includeExts(listing, "html", "css", "scss", "sass") foreach { p =>
          val outfile = dst resolve (src relativize p)

          otherTemplates += TemplateFile(outfile, null, templateParser.parse(readFile(p.toString)))
        }

      if (dir startsWith static) {
        if (static == src) {
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
      } else {
        //
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

  def withoutExtension(filename: String): String =
    filename lastIndexOf '.' match {
      case -1  => filename
      case dot => filename substring (0, dot)
    }

}

case class Data(parent: Path, name: String, data: Any)

case class ContentFile(outdir: Path, name: String, page: Any, source: String, var content: String, var toc: TOC)

case class TemplateFile(path: Path, name: String, template: TemplateAST)

case class Site(content: List[ContentFile],
                data: List[Data],
                layoutTemplates: List[TemplateFile],
                partialTemplates: List[TemplateFile],
                shortcodeTemplates: List[TemplateFile],
                otherTemplates: List[TemplateFile])
