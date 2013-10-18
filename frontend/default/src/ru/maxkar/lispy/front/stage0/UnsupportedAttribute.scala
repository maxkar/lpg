package ru.maxkar.lispy.front.stage0

import ru.maxkar.lispy.front.Attributes

/** Unsupported attribute exception. */
final case class UnsupportedAttribute(key : String,
    location : Attributes) extends
  Exception("Unsupported attribute " + key + " at " + location)
