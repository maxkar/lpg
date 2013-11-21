package ru.maxkar.jssample.att

import ru.maxkar.jssample.doc._

import ru.maxkar.lispy._
import ru.maxkar.lispy.parser._

/** Documentation attribute. */
final object Doc {
  /** Documentation attribute. */
  val ATTR = new Attribute[DocBody]("Element's documentation")


  /** Checks, if character is nonspecial. */
  private def isNonspecial(x : Char) : Boolean =
    x match {
      case '\\' | '}' ⇒  false
      case _ ⇒  true
    }


  /** Parses an attribute. */
  def parse(start : Attributes, inp : Input) : Attributes = {
    val res = new StringBuilder

    while (true) {
      res ++= inp.charsWhile(isNonspecial).replaceAll("\\+", " ")

      inp.peek match {
        case x if x < 0 ⇒ return Attributes.empty
        case '\\' ⇒
          inp.dropN(1)
          if (inp.peek >= 0) {
            res += inp.peek.asInstanceOf[Char]
            inp.dropN(1)
          } else
            return Attributes.empty
        case '}' ⇒
          return Attributes.singleton(ATTR, DocBody.text(res.toString))
        case _ ⇒
          throw new AssertionError("Bad peek " + inp.peek)
      }
    }
    Attributes.empty
  }
}
