package io.github.edadma.juice

case class Ident(pos: Int, name: String)

trait ShortcodeParserAST

case class ShortcodeStartAST(name: Ident, attrs: List[(Ident, String)], closed: Boolean) extends ShortcodeParserAST

case class ShortcodeEndAST(name: Ident) extends ShortcodeParserAST
