//package io.github.edadma.juicer
//
//import scala.jdk.CollectionConverters._
//import io.github.edadma.cross_platform._
//import io.github.edadma.squiggly._
//
//import java.nio.file.{Files, Path}
//import scala.collection.mutable
//
//class ShortcodeLoader(paths: Seq[Path],
//                      functions: Map[String, TemplateFunction],
//                      namespaces: Map[String, Map[String, TemplateFunction]]) {
//
//  val map = new mutable.HashMap[String, TemplateAST]
//
//  def find(name: String): Option[TemplateAST] =
//    map get name match {
//      case None =>
//        for (d <- paths)
//          for (f <- Files.list(d).iterator().asScala) {
//            val path = f.toString
//
//            if (readableFile(path)) {
//              val filename = f.getFileName.toString
//
//              if (filename lastIndexOf '.' match {
//                    case -1  => filename == name
//                    case dot => filename.substring(0, dot) == name
//                  }) {
//                val t = new TemplateParser("{{", "}}", functions, namespaces).parse(readFile(path))
//
//                map(name) = t
//                return Some(t)
//              }
//            }
//          }
//
//        None
//      case Some(template) => Some(template)
//    }
//
//}
