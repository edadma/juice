package io.github.edadma.juice

import io.github.edadma.scemplate

import java.nio.file.Paths

object Main extends App {

  val functions = scemplate.Builtin.functions
  val namespaces = scemplate.Builtin.namespaces
  val shortcodeLoader = new Loader(List(Paths.get("test")), functions, namespaces)
  val renderer = new scemplate.Renderer(functions)
  val preproc = new Preprocessor("{<", ">}", shortcodeLoader, renderer)
  val input =
    """
      |YouTube Video
      |=============
      |
      |{< youtube code=09jf3ow9jfw />}
      |""".stripMargin
  val output = preproc.process(input)

  println(output)

}
