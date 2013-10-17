package ru.maxkar.lispy.front.stage0

import ru.maxkar.lispy.front.Attributes

/** Unclosed string error.
 * @param start string start.
 * @param err error location.
 */
final class UclosedString(val start : Attributes, val err : Attributes)
  extends Exception("Unclosed string at " + err + " started at " + start)
