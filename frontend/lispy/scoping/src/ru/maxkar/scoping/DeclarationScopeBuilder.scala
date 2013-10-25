package ru.maxkar.scoping

import ru.maxkar.lispy._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

/** Builder of declaration scope. */
private[scoping] final class DeclarationScopeBuilder(
    list : ArrayBuffer[DuplicateDeclarationInfo]) {

  import ru.maxkar.lispy.SPattern._

  /** Local declarations map. */
  private val locals = new HashMap[String, Attributes]


  /** Adds an item if it is a simple name. */
  private def addSimpleName(expr : SExpression[BaseItem]) : Unit =
    expr match {
      case IdLeaf(i, l) ⇒ offerLocal(i, l)
      case _ ⇒  ()
    }


  /** Adds a name if it is in a "variable" form. */
  private def addVarName(expr : SExpression[BaseItem]) : Unit =
    expr match {
      case IdLeaf(i, l) ⇒ offerLocal(i, l)
      case IdList(n, a, _) ⇒ offerLocal(n, a)
      case _ ⇒  ()
    }


  /** Processes one declaration. */
  def processDeclaration(item : SExpression[BaseItem]) : Unit = {
    item match {
      case IdList("const", _, tl) ⇒ tl.foreach(addSimpleName)
      case IdList("var", _, tl) ⇒ tl.foreach(addVarName)
      case IdList("def", _, Seq(IdLeaf(n, a), _*)) ⇒ offerLocal(n, a)
      case _ ⇒ ()
    }
  }


  /** Offers a new item. */
  def offerLocal(key : String, loc : Attributes) : Unit = {
    locals.get(key) match {
      case Some(x) ⇒ list += DuplicateDeclarationInfo(key, x, loc)
      case None ⇒ locals.put(key, loc)
    }
  }


  /** Returns set of found names. */
  def names() : Set[String] = locals.keys.toSet
}
