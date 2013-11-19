package ru.maxkar.jssample.msg

import ru.maxkar.lispy.parser._
import ru.maxkar.lispy._

import ru.maxkar.jssample.ns._
import ru.maxkar.jssample.att._

/** Callback for events in one compilation host (source). */
trait HostTrace {

  /** Adds a new message. */
  protected def msg(message : Message) : Unit


  /** Returns host of this trace. */
  protected val host : java.io.File


  /** Notifies about a host reading failure. */
  def readFailure(cause : java.io.IOException) : Unit =
    msg(ReadFailure(host, cause))


  /** Notifies about a parsing failure. */
  def parseFailure(cause : SFormatException) : Unit =
    msg(MailformedFile(host, cause))


  /** Notifies about mailformed attribute. */
  def mailformedAttribute(cause : MailformedAttribute) : Unit =
    msg(MailformedEltAttribute(host, cause))


  /** Notifies about a duplicate declaration. */
  def duplicateDeclaration(name : String, first : TextPosition, next : TextPosition) : Unit =
    msg(DuplicateDeclaration(host, name, first, next))


  /** Notifies about a mailformed declaration. */
  def mailformedDeclaration(decl : SExpression[BaseItem]) : Unit =
    msg(MailformedDeclaration(host, locOf(decl)))


  /** Notifies about an undeclared identifier. */
  def undeclaredIdentifier(defn : SExpression[BaseItem]) : Unit =
    msg(UndeclaredIdentifier(host, locOf(defn)))


  /** Notifies about ambigious identifiers. */
  def ambigiousIdentifier(defn : SExpression[BaseItem], others : Seq[DeclarationHost]) : Unit =
    msg(AmbigiousIdentifier(host, locOf(defn), others))


  /** Notifies about bad expression. */
  def badExpression(defn : SExpression[BaseItem]) : Unit =
    msg(BadExpression(host, locOf(defn)))


  /** Definitely uncallable expression call. */
  def uncallableExpression(expr : SExpression[BaseItem]) : Unit =
    msg(UncallableExpression(host, locOf(expr)))


  /** Unassignable expression in assign position. */
  def unassignableExpression(expr : SExpression[BaseItem]) : Unit =
    msg(UnassignableExpression(host, locOf(expr)))


  /** Duplicate access modifier for an item at given position. */
  def duplicateAccessDefinition(expr : SExpression[BaseItem]) : Unit =
    msg(DuplicateAccessDefinition(host, locOf(expr)))


  /** Multiple from statements. */
  def duplicateFromAttribute(expr : SExpression[BaseItem]) : Unit =
    msg(DuplicateFromAttribute(host, locOf(expr)))


  /** Bad reference to a module. */
  def badModuleReference(loc : TextPosition) : Unit =
    msg(BadModuleReference(host, loc))


  /** Calculates a location of the expression. */
  private def locOf(x : SExpression[BaseItem]) : TextPosition = {
    val slist = x.atts.allValues(Input.textPosition)
    if (slist.isEmpty)
      null
    else
      slist.iterator.next
  }
}
