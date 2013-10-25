package ru.maxkar.jssample.stage0


import java.io._

import ru.maxkar.lispy.parser._
import ru.maxkar.lispy._


/** Stage-0 messages. */
final object Msg {
  import ru.maxkar.jssample.MessageFormat._


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


  /** Prints a decoded exception. */
  def printException(stream : PrintStream, exn : Failure) : Unit = {
    exn match {
      case ReadFailure(f, e) ⇒
        stream.print(exn.file)
        stream.println(": Failed to read file : " + exn)
        e.printStackTrace(stream)
      case SFormatFailure(f, e) ⇒
        stream.println(err(f, e.location, formatSExceptionBody(e)))
    }
  }
}
