package io.github.edadma.juicer

case class Ident(pos: Int, name: String)

trait ShortcodeParserAST

case class ShortcodeStartAST(name: Ident, attrs: Seq[(Ident, Option[String])], closed: Boolean)
    extends ShortcodeParserAST

case class ShortcodeEndAST(name: Ident) extends ShortcodeParserAST
