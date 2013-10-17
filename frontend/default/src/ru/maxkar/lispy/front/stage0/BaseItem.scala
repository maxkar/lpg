package ru.maxkar.lispy.front.stage0

import java.math._

/**
 * Basic/base element for expressions.
 * These elements are building blocks which are used
 * with s-expression glue to create solid program
 * constructs.
 */
abstract sealed class BaseItem

/**
 * Simple identifier. These identifiers may include
 * special characters (like +, -, etc...). So they are more
 * like lisp identifiers/atoms, not a "common-sense" id.
 * <p>Identifier can't start with minus sign (&apos;-&apos;) and
 *   have a digit as it's second character. "-1" is not valid
 *   identifier. However, "--1" is valid identifier.
 * <p>Example identifiers: abc, xyz, a-b-c, op+, ++, --.
 */
final case class BaseId(name : String) extends BaseItem

/**
 * String literal. Like a common string in most languages.
 * This literals support multiline strings. In this case
 * each next line must start with a quote character.
 * <p>Multiline example:
 * <pre>
 *  "This is
 *  " a
 *  " three-line string"
 * </pre>
 */
final case class BaseString(value : String) extends BaseItem

/**
 * Integer literal. Just plain and simple number literal.
 * May start with a minus sign. Due to the nature of this module
 * (base unrestricted parsing) this class uses biginteger as its
 * internal representation.
 */
final case class BaseInteger(value : BigInteger) extends BaseItem

/**
 * Floating-point literal. Due to a nature of this module,
 * they are represented as mantissa and exponent with
 * "unlimited" precision. Implementations are encouraged to
 * trim values and store regular double (or plain BigDecimal)
 * values in attributes of the AST.
 */
final case class BaseFloating(
  mantissa : BigDecimal, exponent : BigInteger) extends BaseItem
