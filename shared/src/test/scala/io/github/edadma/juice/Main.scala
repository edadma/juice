package io.github.edadma.juice

import io.github.edadma.scemplate

object Main extends App {

  val youtubeTemplate =
    """
      |<div class="embed video-player">
      |<iframe class="youtube-player" type="text/html" width="640" height="385" src="https://www.youtube.com/embed/{{ index .Params 0 }}" allowfullscreen frameborder="0">
      |</iframe>
      |</div>
      |""".stripMargin
  val md =
    """
      |YouTube Video
      |=============
      |
      |{< youtube code=09jf3ow9jfw />}
      |""".stripMargin
  val shortcodes = Map("youtube" ->)
  val functions = scemplate.Builtin.functions
  val renderer = new scemplate.Renderer(functions)
  val preproc = new Preprocessor("{<", ">}", shortcodes, renderer)

  println("> cross_templateJVM/Test/run")

}
