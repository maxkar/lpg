package ru.maxkar.jssample.stage0


import java.io._

import ru.maxkar.lispy.parser._
import ru.maxkar.lispy._


/** Stage-0 messages. */
final object Msg {

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


  /** Formats a source location. */
  private def formatLocation(atts : Attributes) : String = {
    val aset = atts.allValues(Input.textPosition)
    if (aset.size != 1)
      return "<unknown/unsupported location>"
    val x = aset.head
    "(" + x.line + "," + x.column + ")"
  }


  /** Formats an S-expression. */
  private def formatSException(exn : SFormatException) : String = {
    formatLocation(exn.location) + " : " + formatSExceptionBody(exn)
  }



  /** Prints a decoded exception. */
  def printException(stream : PrintStream, exn : Failure) : Unit = {
    stream.print(exn.file)
    exn match {
      case ReadFailure(f, e) ⇒
        stream.println(": Failed to read file : " + exn)
        e.printStackTrace(stream)
      case SFormatFailure(f, e) ⇒
        stream.println(formatSException(e))
    }
  }
}
