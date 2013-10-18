package ru.maxkar.lispy.front.stage0

import ru.maxkar.lispy.front._
import ru.maxkar.lispy.front.parser._


/** Attributes parser implementation. */
private class AttsParser(
    factory : String ⇒ Option[AttsParser.ContentParser]) {


  /** Performs an actual parsing. */
  private def parse(input : Input) : Attributes = {
    var res = Attributes.empty

    while (true) {
      input.dropWhites

      val sloc = input.openingAttributes
      if (input.peek != '{')
        return res

      input.dropN(1)
      input.dropWhites
      val key = input.charsWhile(Character.isJavaIdentifierPart)
      input.dropWhites

      if (input.peek == ':') {
        input.dropN(1)
        input.dropWhites
      }

      factory(key) match {
        case None ⇒ throw new UnsupportedAttribute(key, sloc)
        case Some(x) ⇒
          res = res ++ x(sloc, input)
          input.dropWhites
          if (input.peek != '}')
            throw new UnclosedAttribute(sloc, input.openingAttributes)
          input.dropN(1)
      }
    }

    res
  }
}



/**
 * Attributes parser extensions.
 */
object AttsParser {
  /** Attribute content parser.
   * It receives a "starting position" attributes and
   * input and provides content of the attribute.
   * Usually it produces a singleton attribute, but
   * such behavior is not mandatory and parser may
   * provide several attributes or no attributes at all.
   */
  type ContentParser = (Attributes, Input) ⇒ Attributes


  /**
   * Creates attribute parser from factory.
   * @param factory concrete parser lookup function.
   * @return function, which parses a serie of attributes
   * in the input.
   */
  def fromFactory(
        factory : String ⇒ Option[ContentParser]) :
      Input ⇒ Attributes = {
    new AttsParser(factory).parse
  }
}
