package ru.maxkar.lispy.front.stage0

import ru.maxkar.lispy.front.Attributes

final class BadSExpression(val loc : Attributes)
  extends Exception("Mailformed S-expression at " + loc)
