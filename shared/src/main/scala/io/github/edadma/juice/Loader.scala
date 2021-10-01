package io.github.edadma.juice

import scala.jdk.CollectionConverters._

import io.github.edadma.scemplate.{BuiltinFunction, TemplateParser, TemplateParserAST}

import io.github.edadma.cross_platform._

import java.nio.file.{Files, Path}
import scala.collection.mutable

class Loader(paths: Seq[Path],
             functions: Map[String, BuiltinFunction],
             namespaces: Map[String, Map[String, BuiltinFunction]]) {

  val map = new mutable.HashMap[String, TemplateParserAST]

  def find(name: String): Option[TemplateParserAST] =
    map get name match {
      case None =>
        for (d <- paths)
          for (f <- Files.list(d).iterator().asScala) {
            val path = f.toString

            if (readableFile(path)) {
              val filename = f.getFileName.toString

              if (filename lastIndexOf '.' match {
                    case -1  => filename == name
                    case dot => filename.substring(0, dot) == name
                  }) {
                val t = new TemplateParser(readFile(path), "{{", "}}", functions, namespaces).parse

                map(name) = t
                return Some(t)
              }
            }
          }

        None
      case Some(template) => Some(template)
    }

}
