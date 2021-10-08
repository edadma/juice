package io.github.edadma.juice

import io.github.edadma.char_reader._
import io.github.edadma.squiggly._

import java.io.{ByteArrayOutputStream, PrintStream}
import scala.annotation.tailrec
import scala.language.postfixOps

class Preprocessor(startDelim: String, endDelim: String, shortcodes: Loader, renderer: TemplateRenderer) {

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

                shortcodes find name match {
                  case Some(template) =>
                    val code = new ByteArrayOutputStream
                    val out = new PrintStream(code)

                    renderer.render(data, template, out)
                    buf ++= code.toString
                  case None => r.error(s"unknown shortcode: $name")
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
