package io.github.edadma.juicer

import io.github.edadma.squiggly
import io.github.edadma.commonmark
import io.github.edadma.commonmark.{CommonMarkParser, Util}
import org.ekrich.config._

import scala.jdk.CollectionConverters._
import java.nio.file.Paths
import scala.annotation.tailrec

object Main extends App {

  def clean(s: String): String = {
    val buf = new StringBuilder(s)

    while (buf.nonEmpty && buf.head.isDigit) buf.deleteCharAt(0)
    while (buf.nonEmpty && !buf.head.isLetterOrDigit) buf.deleteCharAt(0)

    @tailrec
    def clean(from: Int): Unit =
      buf.indexWhere(!_.isLetterOrDigit, from) match {
        case -1 =>
        case idx =>
          buf.indexWhere(_.isLetterOrDigit, idx) match {
            case -1 =>
              buf.delete(idx, buf.length)
            case end =>
              buf(idx) = '-'
              buf.delete(idx + 1, end)
              clean(idx + 1)
          }
      }

    clean(0)

    if (buf.isEmpty) "-"
    else buf.toString
  }

  println(s"|${clean("123-as -- df   ---")}|")

}
