package io.github.edadma.juice

import org.ekrich.config._

object Main extends App {

  val conf = ConfigFactory.parseString(
    """
      |baseurl = http://localhost:8080
      |""".stripMargin,
    ConfigParseOptions.defaults.setSyntax(ConfigSyntax.PROPERTIES)
  )

  println(conf.getString("baseurl"))

}
