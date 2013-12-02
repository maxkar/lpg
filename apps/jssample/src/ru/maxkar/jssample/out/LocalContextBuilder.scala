package ru.maxkar.jssample.out

import ru.maxkar.scoping.simple._

import ru.maxkar.jssample.msg.HostTrace

import ru.maxkar.backend.js.model._
import ru.maxkar.lispy._

/** Builder for the local context. */
private[out] final class LocalContextBuilder(
    val trace : HostTrace,
    val host : java.io.File,
    root : RootScopeBuilder) {


  /** Variable builder. */
  private val varb = ScopeBuilder.withCallback[String, Symbol](
    (k, s1, s2) ⇒
      trace.duplicateDeclaration(k,
        s1.declaration.offset,
        s2.declaration.offset))


  /** Locals builder. */
  val labb = ScopeBuilder.withCallback[String, Symbol](
    (k, s1, s2) ⇒
      trace.duplicateDeclaration(k,
        s1.declaration.offset,
        s2.declaration.offset))


  /** Adds a new argument. */
  def mkArg(name : String, item : SExpression[BaseItem]) : Symbol = {
    val res = root.mkArg(item)
    varb.offer(name, res)
    res
  }


  /** Adds a new local variable. */
  def mkVar(name : String, item : SExpression[BaseItem]) : Symbol = {
    val res = root.mkVar(item)
    varb.offer(name, res)
    res
  }


  /** Creates an anonymous variable. */
  def mkAnonVar(decl : SExpression[BaseItem]) : Symbol = root.mkVar(decl)


  /** Creates a new automatic variable. */
  def mkAutoVar(name : String, decl : SExpression[BaseItem]) : Symbol = {
    val res = root.mkAutoVar(decl)
    varb.offer(name, res)
    res
  }


  /** Adds a new label. */
  def mkLabel(name : String, item : SExpression[BaseItem]) : Symbol = {
    val res = root.mkLabel(item)
    labb.offer(name, res)
    res
  }


  /** Creates a new subcontext. */
  def newSubBuilder() : LocalContextBuilder =
    new LocalContextBuilder(trace, host, root)


  /** Ends a building. */
  def end(parent : SymbolScope, funcImpl : (Symbol, FunctionBody) ⇒ Unit) : LocalContext =
    new LocalContext(parent.subscope(varb.scope), labb.scope, trace, host, funcImpl)


  /** Ends in a related context. */
  def endRelated(other : LocalContext) : LocalContext =
    new LocalContext(
      other.variables.subscope(varb.scope),
      Scope.chain(other.labels, labb.scope),
      trace, host, other.funcImpl)
}
