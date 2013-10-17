package ru.maxkar.lispy.front.stage0


import ru.maxkar.lispy.front.Attributes

/** Mailformed escape character error.
 * @param err error location.
 */
final class BadEscapeChar(val err : Attributes)
  extends Exception("Bad escape char at " + err)

