package ru.maxkar.jssample.out

import ru.maxkar.lispy._

import ru.maxkar.scoping.simple._
import ru.maxkar.jssample.ns._

import ru.maxkar.jssample.msg.HostTrace

import ru.maxkar.jssample.att.Use

/** Scope with symbols. */
private[out] final class SymbolScope(
      syms : Scope[String, Symbol],
      selfScope : Scope[String, Symbol],
      mods : Scope[String, Premodule],
      trace : HostTrace) {


  /** Lookups a "raw" item with all possible values. */
  def lookupAll(key : String, item : SExpression[BaseItem]) : Set[Symbol] = {
    findLookupContext(item) match {
      case None ⇒  Set.empty
      case Some(x) ⇒ x.lookup(key)
    }
  }


  /** Lookups a module. */
  private def lookupModuleScope(r : ModuleRef) : Option[Scope[String, Symbol]] = {
    val ms = mods.lookup(r.name)
    if (ms.size != 1) {
      trace.badModuleReference(r.loc)
      None
    } else
      Some(ms.head.publicScope)
  }



  /** Lookups a context for the item. */
  private def findLookupContext(item : SExpression[BaseItem]) : Option[Scope[String, Symbol]] = {
    val lcontext = item.atts.allValues(Use.FROM)

    if (lcontext.isEmpty)
      return Some(syms)

    if (lcontext.size > 1) {
      trace.duplicateFromAttribute(item)
      return Some(syms)
    }

    lcontext.head match {
      case None ⇒ Some(selfScope)
      case Some(x) ⇒ lookupModuleScope(x)
    }
  }


  /** Lookup simple symbol. */
  def lookup(key : String, item : SExpression[BaseItem]) : Option[Symbol] = {
    findLookupContext(item) match {
      case None ⇒  None
      case Some(x) ⇒
        val  guess = x.lookup(key)
        if (guess.isEmpty) {
          trace.undeclaredIdentifier(item)
          None
        } else if (guess.size > 1) {
          trace.ambigiousIdentifier(item, guess.toSeq.map(_.declaration))
          None
        } else
          Some(guess.head)
    }
  }


  /** Lookups a "guaranteed" symbol. */
  def lookupGuaranteed(item : String) : Symbol =
    syms.lookup(item).head


  /** Adds items and returns a new subscope. */
  def subscope(subsyms : Scope[String, Symbol]) : SymbolScope =
    new SymbolScope(Scope.chain(syms, subsyms), selfScope, mods, trace)


  /** Creates a new context for a nested scope. */
  def forNested(item : SExpression[BaseItem]) : SymbolScope = {
    val modNames = item.atts.allValues(Use.MODULE).flatMap(lookupModuleScope).toSeq
    if (modNames.isEmpty)
      return this

    val imported = Scope.join(modNames :_*)
    subscope(imported)
  }
}
