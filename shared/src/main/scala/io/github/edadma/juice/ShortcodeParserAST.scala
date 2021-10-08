package io.github.edadma.juice

case class Ident(pos: Int, name: String)

trait ShortcodeTemplateAST

case class ShortcodeStartAST(name: Ident, attrs: Seq[(Ident, Option[String])], closed: Boolean)
    extends ShortcodeTemplateAST

case class ShortcodeEndAST(name: Ident) extends ShortcodeParserAST
