package io.github.edadma.juice

import io.github.edadma.char_reader._

object Preprocessor {

  def process(content: String): String = {
    val buf = new StringBuilder

    def process(r: CharReader): Unit =
      if (r.more) {}

    process(CharReader.fromString(content))
    buf.toString
  }

}
