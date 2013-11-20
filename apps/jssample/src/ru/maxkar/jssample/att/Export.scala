package ru.maxkar.jssample.att

import ru.maxkar.lispy._
import ru.maxkar.lispy.parser._

/** Export attribute. This attribute is independent from the
 * access specifier. For example, "private" function may be
 * exported into a global scope. This may be usefull to provide
 * some functions which are used on the page scripts but are
 * not available for other modules.
 */
object Export {
  /** Export attribute. */
  val ATTR = new Attribute[Option[String]]("Export specification")


  /** Parses an attribute. */
  def parse(start : Attributes, inp : Input) : Attributes = {
    inp.dropWhites
    if (inp.peek < 0 || !Character.isJavaIdentifierStart(inp.peek))
      return Attributes.singleton(ATTR, None)

    val tok = inp.charsWhile(Character.isJavaIdentifierPart)
      return Attributes.singleton(ATTR, Some(tok))
  }
}
