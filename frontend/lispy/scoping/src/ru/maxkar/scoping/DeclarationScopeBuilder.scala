package ru.maxkar.scoping

import ru.maxkar.lispy._

import scala.collection.mutable.ArrayBuffer
import ru.maxkar.alias.collection._

/** Builder of declaration scope. */
private[scoping] final class DeclarationScopeBuilder(
    list : ArrayBuffer[DuplicateDeclarationInfo]) {

  import ru.maxkar.lispy.SPattern._

  /** Local declarations map. */
  private val locals = new JHashMap[String, Attributes]


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
    val prev = locals.put(key, loc)
    if (prev != null) {
      list += DuplicateDeclarationInfo(key, prev, loc)
      locals.put(key, loc)
    }
  }


  /** Returns set of found names. */
  def names() : JSet[String] = java.util.Collections.unmodifiableSet(locals.keySet)
}
