package io.github.edadma.juice

import org.ekrich.config.{Config, ConfigFactory, ConfigParseOptions, ConfigSyntax}

object BaseConfig {

  private val configs =
    Map(
      "basic" -> ("""
          |baseURL =    
          |title =      Untitled
          |author =     Noname
          |content =    .
          |pagelayout = page
          |layouts =    .
          |shortcodes = .
          |partials =   .
          |""".stripMargin, ConfigSyntax.PROPERTIES)
    )

  def apply(name: String): Option[Config] =
    configs get name map {
      case (c, s) => ConfigFactory.parseString(c, ConfigParseOptions.defaults.setSyntax(s))
    }

}

/*
    """
      |baseURL =         "http://localhost:8000"
      |title =           Untitled
      |author =          Noname
      |languagecode =    en-us
      |contentdir =      .
      |layoutdir =       .
      |shortcodedir =    .
      |partialdir =      .
      |staticdir =       .
      |resourcedir =     resources
      |themesdir =       themes
      |cachedir =        /tmp/juice_cache
      |ignorefiles =     []
      |""".stripMargin,
 */
