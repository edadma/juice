package io.github.edadma.juicer

import org.parboiled2._
import shapeless.HNil

import scala.language.implicitConversions
import scala.util.{Failure, Success}

class ShortcodeParser(val input: ParserInput, line: Int, col: Int) extends Parser {

  implicit def wsStr(s: String): Rule0 = rule(str(s) ~ sp)

  def shortcode: Rule1[ShortcodeParserAST] = rule(sp ~ (shortcodeStart | shortcodeEnd) ~ EOI)

  def closed: Rule1[Boolean] = rule("/" ~ push(true) | push(false))

  def value: Rule1[String] = rule(singleQuoteString | doubleQuoteString | unquotedString)

  def parameter: Rule1[ShortcodeParameter] = rule(ident ~ "=" ~ value ~> NamedParameter | value ~> PositionalParameter)

  def shortcodeStart: Rule1[ShortcodeStartAST] = rule(ident ~ zeroOrMore(parameter) ~ closed ~> ShortcodeStartAST)

  def shortcodeEnd: Rule1[ShortcodeEndAST] = rule("/" ~ ident ~> ShortcodeEndAST)

  def singleQuoteString: Rule1[String] = rule('\'' ~ capture(zeroOrMore("\\'" | noneOf("'\n"))) ~ '\'' ~ sp)

  def doubleQuoteString: Rule1[String] = rule('"' ~ capture(zeroOrMore("\\\"" | noneOf("\"\n"))) ~ '"' ~ sp)

  def unquotedString: Rule1[String] =
    rule(
      capture((CharPredicate.AlphaNum | '_' | '-' | ':') ~ zeroOrMore(CharPredicate.AlphaNum | '_' | '-' | ':' |
        test(charAtRC(1).isLetterOrDigit || charAtRC(1) == '-' || charAtRC(1) == '_') ~ '/')) ~ sp)

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
