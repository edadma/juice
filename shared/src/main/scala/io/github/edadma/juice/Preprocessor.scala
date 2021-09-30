package io.github.edadma.juice

import io.github.edadma.char_reader._
import io.github.edadma.scemplate.{BuiltinFunction, TemplateParserAST}

class Preprocessor(startDelim: String,
                   endDelim: String,
                   shortcodes: Map[String, TemplateParserAST],
                   functions: Map[String, BuiltinFunction]) {

  def process(content: String): String = {
    val buf = new StringBuilder

    def process(r: CharReader): Unit =
      if (r.more)
        r.matchDelimited(startDelim, endDelim) match {
          case Some((shortcode, rest)) =>

          case Some((ShortcodeStartAST(Ident(_, name), attrs, closed)) =>
          case None =>
            buf += r.ch
            process(r.next)
        }

    process(CharReader.fromString(content))
    buf.toString
  }

}
