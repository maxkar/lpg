package ru.maxkar.jssample.att

import ru.maxkar.lispy._
import ru.maxkar.lispy.parser._


/** Variable arity argument for the function. */
final object Vararg {
  /** Vararg attribute specifier. */
  val ATTR = new Attribute[Unit]("Variable-arity function specifier")

  def parse(start : Attributes, inp : Input) : Attributes =
    Attributes.singleton(ATTR, ())
}
