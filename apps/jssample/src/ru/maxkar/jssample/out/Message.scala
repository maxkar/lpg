package ru.maxkar.jssample.out

import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.jssample.ns._


import java.io.File

/** Processing message. */
abstract sealed class Message

/** Duplicate declaration message. */
final case class DuplicateDeclaration(host : File, name : String,
    fst : TextPosition, snd : TextPosition) extends Message


/** Illegal declaration. */
final case class BadDeclaration(host : File, pos : TextPosition) extends Message


/** Message utils. */
object Message {
  import ru.maxkar.jssample.MessageFormat._
  import java.io._

  def printMsg(stream : PrintStream, msg : Message) : Unit = {
    msg match {
      case DuplicateDeclaration(host, name, fst, snd) ⇒
        stream.println(err(host, snd, "Duplicate declaration, first declared at " +
          formatLocation(fst)))
      case BadDeclaration(host, pos) ⇒
        stream.println(err(host, pos, "Bad top-level declaration"))
    }
  }
}
