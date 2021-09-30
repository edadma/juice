package io.github.edadma.juice

import io.github.edadma.scemplate

object Main extends App {

  val youtubeTemplate =
    """
      |<div class="embed video-player">
      |  <iframe class="youtube-player" type="text/html" width="640" height="385" src="https://www.youtube.com/embed/{{ .code }}" allowfullscreen frameborder="0">
      |  </iframe>
      |</div>
      |""".trim.stripMargin
  val functions = scemplate.Builtin.functions
  val namespaces = scemplate.Builtin.namespaces
  val youtubeTemplateAST = new scemplate.TemplateParser(youtubeTemplate, "{{", "}}", functions, namespaces).parse
  val shortcodes = Map("youtube" -> youtubeTemplateAST)
  val renderer = new scemplate.Renderer(functions)
  val preproc = new Preprocessor("{<", ">}", shortcodes, renderer)
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
