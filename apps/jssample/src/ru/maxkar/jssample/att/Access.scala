package ru.maxkar.jssample.att

import ru.maxkar.lispy._
import ru.maxkar.lispy.parser._


/** Element access modifier. */
abstract sealed class Access


/** Private access, item is private to the module. */
final case object Private extends Access


/** Public access. Can be accessed from other modules. */
final case object Public extends Access


/** Mailformed access exception. */
final case class MailformedAccess(location : Attributes) extends MailformedAttribute(
  "Mailformed access attribute at " + location)


/** Access operations. */
final object Access {
  /** Access attribute specifier. */
  val ATTR = new Attribute[Access]("Access specifier")


  /** Parses an attribute. */
  def parse(start : Attributes, inp : Input) : Attributes = {
    inp.dropWhites
    val tok = inp.charsWhile(Character.isLetter)
    tok match {
      case "private" ⇒ Attributes.singleton(ATTR, Private)
      case "public" ⇒ Attributes.singleton(ATTR, Public)
      case _ ⇒ throw new MailformedAccess(start)
    }
  }
}
