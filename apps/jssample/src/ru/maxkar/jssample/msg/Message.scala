package ru.maxkar.jssample.msg

import java.io.IOException
import java.io.File
import java.io.PrintStream

import ru.maxkar.lispy.parser._
import ru.maxkar.lispy._

import ru.maxkar.jssample.ns._
import ru.maxkar.jssample.att._

/** Simple trace message. */
abstract sealed class Message


/** File reading failure. */
final case class ReadFailure(host : File, cause : IOException) extends Message


/** Mailformed file failure. */
final case class MailformedFile(host : File, cause : SFormatException) extends Message


/** Mailformed attribute failure. */
final case class MailformedEltAttribute(host : File, cause : MailformedAttribute) extends Message


/** Duplicate declaration message. */
final case class DuplicateDeclaration(host : File, name : String,
    fst : TextPosition, snd : TextPosition) extends Message


/** Illegal declaration. */
final case class MailformedDeclaration(host : File, pos : TextPosition) extends Message



/** Reference to undeclared identifier. */
final case class UndeclaredIdentifier(host : File, pos : TextPosition) extends Message


/** Reference to ambigous identifier. */
final case class AmbigiousIdentifier(host : File,
      pos : TextPosition, scope : Seq[DeclarationHost]) extends Message


/** Bad expression. */
final case class BadExpression(host : File, pos : TextPosition) extends Message


/** Uncallable expression. */
final case class UncallableExpression(host : File, pos : TextPosition) extends Message


/** Definitely unassignable expression. */
final case class UnassignableExpression(host : File, pos : TextPosition) extends Message


/** Duplicate access definition. */
final case class DuplicateAccessDefinition(host : File, pos : TextPosition) extends Message


/** Duplicate "from" attribute. */
final case class DuplicateFromAttribute(host : File, pos : TextPosition) extends Message


/** Bad module reference error. */
final case class BadModuleReference(host : File, pos : TextPosition) extends Message


/** Host message utilities. */
object Message {


  /** Formats a source location. */
  private def formatLocation(x : TextPosition) : String =
    "(" + x.line + "," + x.column + ")"


  /** Formats a source location. */
  def formatLocation(atts : Attributes) : String = {
    val aset = atts.allValues(Input.textPosition)
    if (aset.size != 1)
      return "<unknown/unsupported location>"
    formatLocation(aset.head)
  }


  /** Formats an error. */
  def err(file : File, atts : TextPosition, msg : String) : String =
    "ERROR: " + file + " " + formatLocation(atts) + ": " + msg


  /** Formats an s-exception body message. */
  private def formatSExceptionBody(exn : SFormatException) : String = {
    exn match {
      case BadEscapeChar(_) ⇒ "Mailformed escape char"
      case BadExponent(_) ⇒ "Mailformed exponent specifier"
      case BadSExpression(_) ⇒ "S-expresion expected"
      case BadString(st, _) ⇒
        "Mailformed string, started at " + formatLocation(st)
      case TrailingData(_) ⇒ "Trailing data after content"
      case UnclosedAttribute(st, _) ⇒
        "Unclosed attribute, opened at " + formatLocation(st)
      case UnclosedSExpression(st, _) ⇒
        "Unclosed S-expression, opened at " + formatLocation(st)
      case UnclosedString(st, _) ⇒
        "Unclosed string, started at " + formatLocation(st)
      case UnsupportedAttribute(k, _) ⇒
        "Unsupported attribute " + k
    }
  }


  /** Formats an attribute exception message body. */
  private def formatAttrExceptionBody(exn : MailformedAttribute) : String = {
    exn match {
      case MailformedAccess(_) ⇒ "Mailformed access specifier"
    }
  }


  /** Formats a declaration host. */
  private def fmtCandidate(loc : DeclarationHost) : String = {
    loc match {
      case SystemHost ⇒  "  <System>"
      case ModuleHost(f, l) ⇒ "  " + f + formatLocation(l)
    }
  }


  /** Prints an error in a default format. */
  def printDefault(stream : PrintStream, msg : Message) : Unit =
    msg match {
      case ReadFailure(h, c) ⇒
        stream.print("ERROR: ")
        stream.print(h)
        stream.println(": Failed to read file : " + c)
        c.printStackTrace(stream)
      case MailformedFile(h, c) ⇒
        stream.print("ERROR: " + h + " " + formatLocation(c.location) + ": ")
        stream.println(formatSExceptionBody(c))
      case MailformedEltAttribute(h, c) ⇒
        stream.print("ERROR: " + h + " " + formatLocation(c.location) + ": ")
        stream.println(formatAttrExceptionBody(c))
      case DuplicateDeclaration(host, name, fst, snd) ⇒
        stream.println(err(host, snd, "Duplicate declaration, first declared at " +
          formatLocation(fst)))
      case MailformedDeclaration(host, pos) ⇒
        stream.println(err(host, pos, "Mailformed declaration"))
      case UndeclaredIdentifier(host, pos) ⇒
        stream.println(err(host, pos, "Reference to undeclared identifier"))
      case AmbigiousIdentifier(host, pos, scopes) ⇒
        stream.println(err(host, pos, "Ambigious reference, declared at :\n" +
          scopes.map(fmtCandidate).mkString("\n")))
      case BadExpression(host, pos) ⇒
        stream.println(err(host, pos, "Mailformed exprssion"))
      case UncallableExpression(host, pos) ⇒
        stream.println(err(host, pos, "Not callable expression"))
      case UnassignableExpression(host, pos) ⇒
        stream.println(err(host, pos, "Assignment to a non-left-value expression"))
      case DuplicateAccessDefinition(host, pos) ⇒
        stream.println(err(host, pos, "Duplicate access modifier"))
      case DuplicateFromAttribute(host, pos) ⇒
        stream.println(err(host, pos, "Duplicate from attribute"))
      case BadModuleReference(host, pos) ⇒
        stream.println(err(host, pos, "Invalid module reference"))
    }
}
