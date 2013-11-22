package ru.maxkar.jssample.att

import ru.maxkar.jssample.doc._

import ru.maxkar.lispy._
import ru.maxkar.lispy.parser._

/** Mailformed documentation exception. */
final case class MailformedDocumentation(peer : MailformedDoc) extends MailformedAttribute(
    "Mailformed documentation " + peer) {
  val location = peer.loc
}

/** Mailformed documentation. */
abstract sealed class MailformedDoc(msg : String, val loc : Attributes) extends
  Exception(msg + " at " + loc) {
}


/** Mailformed doc code. */
final case class BadDocCode(start : Attributes, _loc : Attributes) extends
  MailformedDoc("Bad documentation code fragment started at " + start, _loc)


/** Unclosed special block. */
final case class UnclosedDocSpecial(start : Attributes, _loc : Attributes) extends
  MailformedDoc("Unclosed special doc started at " + start, _loc)


/** Bad special code. */
final case class BadDocSpecial(name : String, _loc : Attributes) extends
  MailformedDoc("Bad doc speical code " + name, _loc)



/** Documentation attribute. */
final object Doc {
  import scala.collection.mutable.ArrayBuffer
  import DocBody._


  /** Documentation attribute. */
  val ATTR = new Attribute[DocBody]("Element's documentation")


  private def readSubblocks(inp : Input) : Seq[DocBody] = {
    val res = new ArrayBuffer[DocBody]
    while (true) {
      inp.dropWhites

      val estart = inp.location
      if (inp.peek != '{')
        return res

      inp.dropN(1)
      res += blockContent(inp)
      inp.dropWhites
      if (inp.peek != '}')
        throw new UnclosedDocSpecial(estart, inp.location)
      inp.dropN(1)
    }
    res
  }


  /** Parses a block content. */
  private def blockContent(inp : Input) : DocBody = {
    inp.dropWhites

    var res = new ArrayBuffer[DocBody]

    while (true) {
      res += inp.charsWhile(isNonspecial).replaceAll("\\s+", " ")

      inp.peek match {
        case x if x < 0 ⇒  return res
        case '}' ⇒ return res
        case '"' ⇒
          val ss = inp.location
          inp.dropN(1)
          res += code(readString(ss, inp))
        case '{' ⇒
          val subblockStart = inp.location

          inp.dropN(1)
          inp.dropWhites

          val design = inp.charsWhile(Character.isLetter)

          design match {
            case "" ⇒ res += br
            case "b" ⇒
              res += bold(blockContent(inp))
            case "i" ⇒
              res += italic(blockContent(inp))
            case "ol" ⇒
              res += ol(readSubblocks(inp))
            case "ul" ⇒
              res += ul(readSubblocks(inp))
          }
          inp.dropWhites
          if (inp.peek != '}')
            throw new UnclosedDocSpecial(subblockStart, inp.location)
          inp.dropN(1)
        case x ⇒ throw new AssertionError("Bad special " + x)
      }
    }

    res
  }


  /** Reads a string. */
  def readString(start : Attributes, input : Input) : String = {
    val rb = new StringBuilder

    while (true) {
      rb ++= input.charsWhile(isCommonStringChar)

      input.peek match {
        case '\r' | '\n' ⇒
          rb += '\n'
          input.dropWhites
          if (input.peek != '"')
            throw new BadDocCode(start, input.location)
          input.dropN(1)
        case '"' ⇒
          input.dropN(1)
          return rb.toString
        case '\\' ⇒
          input.dropN(1)
          input.peek match {
            case x if x < 0 ⇒ ()
            case x ⇒
              rb += x.asInstanceOf[Char]
              input.dropN(1)
          }
        case _ ⇒
          throw new BadDocCode(start, input.location)
      }
    }

    throw new AssertionError("Truth is broken!")
  }


  private def isCommonStringChar(char : Char) : Boolean = {
    char match {
      case '\\' | '"' | '\r' | '\n' ⇒ false
      case _ ⇒  true
    }
  }



  /** Checks, if character is nonspecial. */
  private def isNonspecial(x : Char) : Boolean =
    x match {
      case '\\' | '{' | '}' | '"' ⇒  false
      case _ ⇒  true
    }


  /** Parses an attribute. */
  def parse(start : Attributes, inp : Input) : Attributes =
    try {
      Attributes.singleton(ATTR, blockContent(inp))
    } catch {
      case e : MailformedDoc ⇒  throw new MailformedDocumentation(e)
    }
}
