package ru.maxkar.scoping

import ru.maxkar.alias.collection._
import ru.maxkar.lispy._
import scala.collection.mutable.ArrayBuffer


/** Declarations processor. */
private final class Declarations {

  /** Messages for this node. */
  private val messages = new ArrayBuffer[DuplicateDeclarationInfo]


  /** Default lifting for list. */
  private def liftDefault(expr : SList[BaseItem]) : SExpression[BaseItem] = {
    val sub = new DeclarationScopeBuilder(messages)

    val itr = expr.items.iterator
    while (itr.hasNext)
      sub.processDeclaration(itr.next)

    val nbody = new Array[SExpression[BaseItem]](expr.items.length)

    var iptr = 0
    val bodyitr = expr.items.iterator

    while (bodyitr.hasNext) {
      nbody(iptr) = liftDecls(bodyitr.next)
      iptr += 1
    }

    val body = nbody.toSeq

    val names = sub.names
    val atts = expr.atts + (Declarations.declaredLocals, names)

    SList(body, atts)
  }


  /** Lifts local decalrations to it's enclosing scope. */
  private def liftDecls(expr : SExpression[BaseItem]) : SExpression[BaseItem] = {
    expr match {
      case SLeaf(_, _) ⇒ expr
      case a@SList(_, _) ⇒ liftDefault(a)
    }
  }
}



/** Declarations definition. */
final object Declarations {


  /** Local declarations attribute. */
  val declaredLocals : Attribute[JSet[String]] =
    new Attribute[JSet[String]]("Declared members' names")


  /** Finds all internal declarations and creates declaredLocals
   * attribute for the containing scope.
   */
  def liftDeclarations(expr : SExpression[BaseItem])
      : (SExpression[BaseItem], Seq[DuplicateDeclarationInfo]) = {

    val decls = new Declarations
    val res = decls.liftDecls(expr)
    (res, decls.messages.toSeq)
  }
}
