package ru.maxkar.lispy.front.stage0

import ru.maxkar.lispy.front.Attributes


/** Maiformed s-expression exception. */
abstract sealed class SFormatException(msg : String) extends Exception(msg) {
  /** Failure location. */
  val location : Attributes
}


/** Mailformed escape character error.
 * @param err error location.
 */
final case class BadEscapeChar(location : Attributes)
  extends SFormatException("Bad escape char at " + location)


/** Mailformed exponent. */
final case class BadExponent(val location : Attributes)
  extends SFormatException("Bad exponent at " + location)


/** Mailformed S-expression (general format). */
final case class BadSExpression(val location : Attributes)
  extends SFormatException("Mailformed S-expression at " + location)


/**
 * Mailformed string content error.
 * @param start string start.
 * @param location error location.
 */
final case class BadString(start : Attributes, location : Attributes)
  extends SFormatException(
    "Mailformed string at " + location + " starting from " + start)


/** Trailing data after the file. */
final case class TrailingData(location : Attributes) extends
  SFormatException("Trailing data after " + location)


/** Unsclosed attribute exception. */
final case class UnclosedAttribute(
      start : Attributes, location : Attributes)
    extends SFormatException("Unclosed attribute at " + location + ", start at " + start)


/** Uclosed S-expression. */
final case class UnclosedSExpression
    (opening : Attributes, location : Attributes)
    extends SFormatException(
      "Unclosed S-Expression at " + location + " started at " + opening)


/** Unclosed string error.
 * @param start string start.
 * @param location error location.
 */
final case class UnclosedString(val start : Attributes, val location : Attributes)
  extends SFormatException("Unclosed string at " + location + " started at " + start)


/** Unsupported attribute exception. */
final case class UnsupportedAttribute(key : String, location : Attributes)
  extends SFormatException("Unsupported attribute " + key + " at " + location)
