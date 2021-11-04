package io.github.edadma.juicer

import org.ekrich.config.{Config, ConfigFactory, ConfigParseOptions, ConfigSyntax}

object BaseConfigs {

  private val configs =
    Map(
      "simple" -> ("""
        |baseURL =        "http://localhost:8080"
        |title =          Untitled
        |author =         Unamed
        |contentDir =     .
        |htmlDir =        ""
        |stripPrefix =    false
        |defaultLayout =  .
        |baseofLayout =   baseof
        |fileLayout =     file
        |folderLayout =   folder
        |folderContent =  _index
        |layoutDir =      .
        |shortcodeDir =   .
        |partialDir =     .
        |staticDir =      .
        |themeDir =       ""       
        |publicDir =      public
        """.stripMargin, ConfigSyntax.CONF),
      "standard" -> ("""
        |baseURL =        "http://localhost:8080"
        |title =          Untitled
        |author =         Unamed
        |contentDir =     content
        |htmlDir =        html
        |stripPrefix =    true
        |defaultLayout =  _default
        |baseofLayout =   baseof
        |fileLayout =     file
        |folderLayout =   folder
        |folderContent =  _index
        |layoutDir =      layouts
        |shortcodeDir =   shortcodes
        |partialDir =     partials
        |staticDir =      static
        |themeDir =       themes
        |publicDir =      public
        """.stripMargin, ConfigSyntax.CONF),
      "norme" -> ("""
        |baseURL =        "http://localhost:8080"
        |title =          Sans titre
        |author =         Sans nom
        |contentDir =     contentu
        |htmlDir =        html
        |stripPrefix =    true
        |baseofLayout =   basede
        |defaultLayout =  _défaut
        |fileLayout =     fichier
        |folderLayout =   dossier
        |folderContent =  _indice
        |layoutDir =      mises-en-page
        |shortcodeDir =   codes-courts
        |partialDir =     partiels
        |staticDir =      statique
        |themeDir =       thèmes
        |publicDir =      public
        """.stripMargin, ConfigSyntax.CONF)
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
