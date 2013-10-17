package ru.maxkar.lispy.front.stage0

import ru.maxkar.lispy.front.Attributes

final class BadExponent(val location : Attributes)
  extends Exception("Bad exponent at " + location)
