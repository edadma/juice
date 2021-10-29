package io.github.edadma.juicer

import org.ekrich.config.{Config, ConfigFactory, ConfigParseOptions, ConfigSyntax}

object BaseConfigs {

  private val configs =
    Map(
      "simple" -> ("""
          |baseURL =        http://localhost:8080
          |title =          Untitle
          |author =         Noname
          |contentDir =     .
          |homeLayout =     home
          |fileLayout =     file
          |folderLayout =   folder
          |layoutDir =      .
          |shortcodeDir =   .
          |partialDir =     .
          |staticDir =      .
          |themeDir =       
          |publicDir =      public
          """.stripMargin, ConfigSyntax.PROPERTIES),
      "standard" -> ("""
         |baseURL =        http://localhost:8080
         |title =          Untitle
         |author =         Noname
         |contentDir =     content
         |homeLayout =     home
         |fileLayout =     file
         |folderLayout =   folder
         |layoutDir =      layouts
         |shortcodeDir =   shortcodes
         |partialDir =     partials
         |staticDir =      static
         |themeDir =       themes
         |publicDir =      public
         """.stripMargin, ConfigSyntax.PROPERTIES)
    )

  def apply(name: String): Option[Config] =
    configs get name map {
      case (c, s) => ConfigFactory.parseString(c, ConfigParseOptions.defaults.setSyntax(s))
    }

}

/*
      "simple" -> ("""
          |baseURLKey =     baseURL
          |titleKey =       title
          |authorKey =      author
          |contentDir =     .
          |pageLayoutName = page
          |layoutDir =      .
          |shortcodeDir =   .
          |partialDir =     .
          |staticDir =      .
          |themeDir =       themes
          |themeKey =       theme
          |publicDir =      public
          """.stripMargin, ConfigSyntax.PROPERTIES)
 */

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
      |cachedir =        /tmp/juicer_cache
      |ignorefiles =     []
      |""".stripMargin,
 */
