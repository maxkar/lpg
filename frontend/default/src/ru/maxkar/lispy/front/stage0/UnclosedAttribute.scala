package ru.maxkar.lispy.front.stage0

import ru.maxkar.lispy.front.Attributes

/** Unsclosed attribute exception. */
final case class UnclosedAttribute(
      start : Attributes, error : Attributes)
    extends Exception("Unclosed attribute at " + error + ", start at " + start)
