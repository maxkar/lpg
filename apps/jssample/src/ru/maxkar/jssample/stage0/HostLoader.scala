package ru.maxkar.jssample.stage0

import ru.maxkar.jssample.msg.HostTrace

import java.io._

import ru.maxkar.lispy._
import ru.maxkar.lispy.parser._


/** Loader for the host file. */
private [stage0] final object HostLoader {
  /** Parses attributes. */
  private def parseAttr(x : Input) : Attributes = {
    Attributes.empty
  }


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
      Some(SParser.parseSFile(parseAttr)(input))
    } catch {
      case e : SFormatException ⇒
        msg.parseFailure(e)
        None
    }
  }
}
