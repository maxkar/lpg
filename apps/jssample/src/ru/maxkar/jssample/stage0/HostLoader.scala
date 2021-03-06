package ru.maxkar.jssample.stage0

import ru.maxkar.jssample.msg.HostTrace

import java.io._

import ru.maxkar.lispy._
import ru.maxkar.lispy.parser._

import ru.maxkar.jssample.att._


/** Loader for the host file. */
private [stage0] final object HostLoader {
  /** Attribute parser. */
  private val attParser = AttsParser.fromFactory(
    Map[String, (Attributes, Input) ⇒  Attributes](
      "private" → Access.parsePrivate,
      "public" → Access.parsePublic,
      "use" → Use.parseUse,
      "from-self" → Use.parseFromSelf,
      "from" → Use.parseFromUse,
      "export" → Export.parse,
      "doc" → Doc.parse,
      "vararg" → Vararg.parse,
      "after" → After.parse
    ).get)


  /** Attempts to load a new host file.
   * Returns some item when there was a success.
   * Otherwise returns None. */
  def read(host : File, msg : HostTrace) : Option[Array[Char]] = {
    try {
      Some(Files.fileAsCharsEnc(host, "UTF-8"))
    } catch {
      case e : IOException ⇒
        msg.readFailure(e)
        None
    }
  }


  /** Parses a content. Returns None if there is an error. */
  def parse(content : Array[Char], msg : HostTrace) : Option[SList[BaseItem]] = {
    val input = Input.fromCharArray(content)
    try {
      Some(SParser.parseSFile(attParser)(input))
    } catch {
      case e : SFormatException ⇒
        msg.parseFailure(e)
        None
      case e : MailformedAttribute ⇒
        msg.mailformedAttribute(e)
        None
    }
  }
}
