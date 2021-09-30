package io.github.edadma.juice

import io.github.edadma.char_reader._
import io.github.edadma.scemplate.{BuiltinFunction, TemplateParserAST}

import scala.annotation.tailrec

class Preprocessor(startDelim: String,
                   endDelim: String,
                   shortcodes: Map[String, TemplateParserAST],
                   functions: Map[String, BuiltinFunction]) {

  def process(content: String): String = {
    val buf = new StringBuilder

    @tailrec
    def process(r: CharReader): Unit =
      if (r.more)
        r.matchDelimited(startDelim, endDelim) match {
          case Some(Some((shortcode, rest))) =>
            new ShortcodeParser(shortcode, r.line, r.col).parseShortcode match {
              case ShortcodeStartAST(Ident(_, name), attrs, closed) =>
              case ShortcodeEndAST(Ident(_, name))                  =>
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
