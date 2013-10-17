package ru.maxkar.lispy.front.stage0


import ru.maxkar.lispy.front.Attributes

/**
 * Mailformed string content error.
 * @param start string start.
 * @param err error location.
 */
final class MailformedString(val start : Attributes, val err : Attributes)
  extends Exception("Mailformed string at " + err + " starting from " + start)

