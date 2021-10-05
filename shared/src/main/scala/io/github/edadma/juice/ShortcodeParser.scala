package io.github.edadma.juice

import org.parboiled2._
import shapeless.HNil

import scala.language.implicitConversions
import scala.util.{Failure, Success}

class ShortcodeParser(val input: ParserInput, line: Int, col: Int) extends Parser {

  implicit def wsStr(s: String): Rule0 = rule(str(s) ~ sp)

  def shortcode: Rule1[ShortcodeParserAST] = rule(sp ~ (shortcodeStart | shortcodeEnd))

  def attribute: Rule1[String] =
    rule("=" ~ (word | singleQuoteString | doubleQuoteString))

  def closed: Rule1[Boolean] = rule("/" ~ push(true) | push(false))

  def shortcodeStart: Rule1[ShortcodeStartAST] =
    rule(
      ident ~ zeroOrMore(ident ~ optional(attribute) ~> Tuple2[Ident, Option[String]] _) ~ closed ~> ShortcodeStartAST)

  def shortcodeEnd: Rule1[ShortcodeEndAST] = rule("/" ~ ident ~> ShortcodeEndAST)

  def singleQuoteString: Rule1[String] = rule('\'' ~ capture(zeroOrMore("\\'" | noneOf("'\n"))) ~ '\'' ~ sp)

  def doubleQuoteString: Rule1[String] = rule('"' ~ capture(zeroOrMore("\\\"" | noneOf("\"\n"))) ~ '"' ~ sp)

  def word: Rule1[String] = rule(capture(oneOrMore(CharPredicate.AlphaNum | '_' | '-')) ~ sp)

  def ident: Rule1[Ident] =
    rule {
      pos ~ capture((CharPredicate.Alpha | '_' | '-') ~ zeroOrMore(CharPredicate.AlphaNum | '_' | '-')) ~ sp ~> Ident
    }

  def pos: Rule1[Int] = rule(push(cursor))

  def sp: Rule0 = rule(quiet(zeroOrMore(anyOf(" \t\r\n"))))

  def parseShortcode: ShortcodeParserAST =
    shortcode.run() match {
      case Success(ast)           => ast
      case Failure(e: ParseError) => sys.error("Shortcode is not valid: " + formatError(e))
      case Failure(e)             => sys.error("Unexpected error during shortcode parsing: " + e)
    }

}
