package io.github.edadma.juicer

import io.github.edadma.char_reader._
import io.github.edadma.squiggly._

import java.io.{ByteArrayOutputStream, PrintStream}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.postfixOps

class Preprocessor(startDelim: String = "{{",
                   endDelim: String = "}}",
                   shortcodes: TemplateLoader,
                   renderer: TemplateRenderer) {

  case class Shortcode(pos: CharReader, name: String, attrs: Seq[(Ident, Option[String])], buf: StringBuilder)

  def process(content: String): String = {
    val buf = new StringBuilder
    val stack = new mutable.Stack[Shortcode]

    @tailrec
    def process(r: CharReader): Unit = {
      def render(name: String, attrs: Seq[(Ident, Option[String])], content: Option[String]): Unit = {
        val data = (attrs map { case (Ident(_, k), v) => k -> v.getOrElse("true") }) ++ (content map (s =>
          List("content" -> s)) getOrElse Nil) toMap

        shortcodes(name) match {
          case Some(template) =>
            val code = new ByteArrayOutputStream
            val out = new PrintStream(code)

            renderer.render(data, template, out)

            (if (stack.isEmpty) buf else stack.top.buf) ++= code.toString
          case None => r.error(s"unknown shortcode: $name")
        }
      }

      if (r.more)
        r.matchDelimited(startDelim, endDelim) match {
          case Some(Some((shortcode, rest))) =>
            new ShortcodeParser(shortcode, r.line, r.col).parseShortcode match {
              case ShortcodeStartAST(Ident(_, name), attrs, closed) =>
                if (closed)
                  render(name, attrs, None)
                else
                  stack push Shortcode(r, name, attrs, new StringBuilder)
              case ShortcodeEndAST(Ident(_, endname)) =>
                val Shortcode(_, name, attrs, buf) = stack.pop()

                if (name != endname)
                  r.error(s"shortcode end tag name does not match start tag name: $name")

                render(name, attrs, Some(buf.toString))
            }

            process(rest)
          case Some(None) =>
            (if (stack.isEmpty) buf else stack.top.buf) += r.ch
            process(r.next)
          case None => r.error("unclosed shortcode tag")
        }
    }

    process(CharReader.fromString(content))

    if (stack.nonEmpty)
      stack.top.pos.error(s"unclosed shortcode body: ${stack.top.name}")

    buf.toString
  }

}
