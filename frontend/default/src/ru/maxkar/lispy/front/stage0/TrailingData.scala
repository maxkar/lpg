package ru.maxkar.lispy.front.stage0

import ru.maxkar.lispy.front.Attributes

final case class TrailingData(loc : Attributes) extends
  Exception("Trailing data after " + loc)
