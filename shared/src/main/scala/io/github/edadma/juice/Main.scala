package io.github.edadma.juice

import org.ekrich.config._

import scala.jdk.CollectionConverters._

object Main extends App {

  val conf =
    ConfigFactory.parseString(
      """
        |baseurl = http://localhost:8080
        |layouts = .
        |mode = basic
        |""".stripMargin,
      ConfigParseOptions.defaults.setSyntax(ConfigSyntax.PROPERTIES)
    )
  val conf1 =
    ConfigFactory.parseString("""
        |baseurl = example.com
        |layouts = [layoutdir1, layoutdir2]
        |""".stripMargin).withFallback(conf)
  val v = conf1.getAnyRef("layouts")

  println(conf1)
  println( /*v.asInstanceOf[java.util.ArrayList[Any]].asScala*/ v, v.getClass)

}
