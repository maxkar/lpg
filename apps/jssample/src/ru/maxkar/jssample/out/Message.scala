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


/** Bad expression. */
final case class BadExpression(host : File, pos : TextPosition) extends Message


/** Reference to undeclared identifier. */
final case class UndeclaredIdentifier(host : File, pos : TextPosition) extends Message


/** Reference to ambigous identifier. */
final case class AmbigiousIdentifier(host : File,
      pos : TextPosition, scope : Seq[DeclarationHost]) extends Message


/** Uncallable expression. */
final case class UncallableExpression(host : File, pos : TextPosition) extends Message


/** Definitely unassignable expression. */
final case class UnassignableExpression(host : File, pos : TextPosition) extends Message


/** Bad function argument. */
final case class BadArgument(host : File, pos : TextPosition) extends Message


/** Message utils. */
object Message {
  import ru.maxkar.jssample.MessageFormat._
  import java.io._

  private def fmtCandidate(loc : DeclarationHost) : String = {
    loc match {
      case SystemHost ⇒  "  <System>"
      case ModuleHost(f, l) ⇒ f + formatLocation(l)
    }
  }


  def printMsg(stream : PrintStream, msg : Message) : Unit = {
    msg match {
      case DuplicateDeclaration(host, name, fst, snd) ⇒
        stream.println(err(host, snd, "Duplicate declaration, first declared at " +
          formatLocation(fst)))
      case BadDeclaration(host, pos) ⇒
        stream.println(err(host, pos, "Bad declaration"))
      case BadExpression(host, pos) ⇒
        stream.println(err(host, pos, "Mailformed exprssion"))
      case UndeclaredIdentifier(host, pos) ⇒
        stream.println(err(host, pos, "Reference to undeclared identifier"))
      case AmbigiousIdentifier(host, pos, scopes) ⇒
        stream.println(err(host, pos, "Ambigious reference, declared at :\n" +
          scopes.map(fmtCandidate).mkString("\n")))
      case UncallableExpression(host, pos) ⇒
        stream.println(err(host, pos, "Not callable expression"))
      case UnassignableExpression(host, pos) ⇒
        stream.println(err(host, pos, "Assignment to a non-left-value expression"))
      case BadArgument(host, pos) ⇒
        stream.println(err(host, pos, "Illegal function argument declaration"))
    }
  }
}
