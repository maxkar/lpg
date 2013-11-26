package ru.maxkar.jssample.att

import ru.maxkar.lispy._
import ru.maxkar.lispy.parser._
import ru.maxkar.jssample.ns._

/** Use other modules attribute. */
final object Use {
  /** Module reference attribute. */
  val MODULE = new Attribute[ModuleRef]("Reference to a module")
  /** Resolve reference to a specific module. */
  val FROM = new Attribute[Option[ModuleRef]]("Specific module for the reference")


  /** Checks, if character is valid letter. */
  def isModuleLetter(chr : Char) : Boolean =
    Character.isLetter(chr) ||
    Character.isDigit(chr) ||
    chr == '.' || chr == '-' ||
    chr == '_'


  /** Parses a "use" attribute. */
  def parseUse(start : Attributes, inp : Input) : Attributes = {
    inp.dropWhites
    val pos = inp.location.allValues(Input.textPosition).head
    val tok = inp.charsWhile(isModuleLetter)
    if (tok.isEmpty)
      throw new MailformedAccess(start)
    Attributes.singleton(MODULE, new ModuleRef(tok, pos))
  }


  /** Parses a "from self" attribute. */
  def parseFromSelf(start : Attributes, inp : Input) : Attributes =
    Attributes.singleton(FROM, None)


  /** Parses a "from use" attribute. */
  def parseFromUse(start : Attributes, inp : Input) : Attributes = {
    inp.dropWhites
    val pos = inp.location.allValues(Input.textPosition).head
    val tok = inp.charsWhile(Character.isLetter)
    if (tok.isEmpty)
      throw new MailformedAccess(start)
    Attributes.singleton(FROM, Some(new ModuleRef(tok, pos)))
  }
}
