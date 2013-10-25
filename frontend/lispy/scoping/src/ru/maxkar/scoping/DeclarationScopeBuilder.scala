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
  private def addSimpleName(expr : SExpression[BaseItem]) : Unit = {
    val g1 = expr.unLeaf
    if (g1 == null)
      return
    val id = g1.unId
    if (id != null)
      offerLocal(id, expr.atts)
  }

  /** Adds a name if it is in a "variable" form. */
  private def addVarName(expr : SExpression[BaseItem]) : Unit = {
    val g1 = expr.unLeaf
    if (g1 != null) {
      val id = g1.unId
      if (id != null)
        offerLocal(id, expr.atts)
      return
    }

    val g2 = expr.unHeadLeaf
    if (g2 != null) {
      val id = g2.unId
      if (id != null)
        offerLocal(id, expr.head.atts)
      return
    }
  }

  /** Processes one declaration. */
  def processDeclaration(item : SExpression[BaseItem]) : Unit = {
    val g1 = item.unHeadLeaf
    if (g1 == null)
      return

    val id = g1.unId
    if (id == null)
      return

    id match {
      case "const" ⇒ item.tail.foreach(addSimpleName)
      case "var" ⇒ item.tail.foreach(addVarName)
      case "def" ⇒
        val dt = item.tail
        if (dt.isEmpty)
          return
        val hid = dt.head.unHeadLeaf
        if (hid == null)
          return
        val nid = hid.unId
        if (nid != null)
          offerLocal(nid, dt.head.atts)
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
