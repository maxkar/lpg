package ru.maxkar.lispy.front.stage0

import ru.maxkar.lispy.front.Attributes

final class UnclosedSExpression
    (val opening : Attributes, val closing : Attributes)
    extends Exception(
      "Unclosed S-Expression at " + closing + " started at " + opening)
