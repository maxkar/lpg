package ru.maxkar.jssample.att

import ru.maxkar.lispy._
import ru.maxkar.lispy.parser._
import ru.maxkar.jssample.ns._

/**
 * Code generation orders. Allows to compile module
 * after another module. Usually this is required to provide proper
 * initialization for global variables.
 */
final object After {
  /** After attribute specification. */
  val ATTR = new Attribute[ModuleRef]("Export specification")


  /** Checks, if character is valid letter. */
  def isModuleLetter(chr : Char) : Boolean =
    Character.isLetter(chr) ||
    Character.isDigit(chr) ||
    chr == '.' || chr == '_' || chr == '-'


  /** Parses an attribute. */
  def parse(start : Attributes, inp : Input) : Attributes = {
    inp.dropWhites
    val pos = inp.location.allValues(Input.textPosition).head
    val tok = inp.charsWhile(isModuleLetter)
    if (tok.isEmpty)
      throw new MailformedAccess(start)
    Attributes.singleton(ATTR, new ModuleRef(tok, pos))
  }
}
