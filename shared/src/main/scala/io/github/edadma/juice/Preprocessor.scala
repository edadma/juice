package io.github.edadma.juice

import io.github.edadma.char_reader._
import io.github.edadma.scemplate

import scala.annotation.tailrec
import scala.language.postfixOps

class Preprocessor(startDelim: String,
                   endDelim: String,
                   shortcodes: Map[String, scemplate.TemplateParserAST],
                   renderer: scemplate.Renderer) {

  def process(content: String): String = {
    val buf = new StringBuilder

    @tailrec
    def process(r: CharReader): Unit =
      if (r.more)
        r.matchDelimited(startDelim, endDelim) match {
          case Some(Some((shortcode, rest))) =>
            new ShortcodeParser(shortcode, r.line, r.col).parseShortcode match {
              case ShortcodeStartAST(Ident(pos, name), attrs, closed) =>
                val data = attrs map { case (Ident(_, k), v) => k -> v.getOrElse("true") } toMap

                shortcodes get name match {
                  case Some(template) => buf ++= renderer.render(data, template)
                  case None           => r.error(s"unknown shortcode: $name")
                }
              case ShortcodeEndAST(Ident(_, name)) =>
            }
          case Some(None) =>
            buf += r.ch
            process(r.next)
          case None => r.error("unclosed shortcode")
        }

    process(CharReader.fromString(content))
    buf.toString
  }

}
