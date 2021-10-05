package io.github.edadma.juice

import io.github.edadma.squiggly
import io.github.edadma.commonmark
import io.github.edadma.commonmark.{CommonMarkParser, Util}

import java.nio.file.Paths

object Main extends App {

  val functions = squiggly.Builtin.functions
  val namespaces = squiggly.Builtin.namespaces
  val shortcodeLoader = new Loader(List(Paths.get("test")), functions, namespaces)
  val renderer = new squiggly.Renderer(null, null, functions)
  val preproc = new Preprocessor("{<", ">}", shortcodeLoader, renderer)
  val input =
    """
      |YouTube Video
      |=============
      |
      |this is a test
      |
      |another paragraph
      |
      |{< youtube code=09jf3ow9jfw />}
      |""".stripMargin
  val md = preproc.process(input)
  val mdparser = new CommonMarkParser
  val doc = mdparser.parse(md)
  val output = Util.html(doc, 2)

  println(output)

}
