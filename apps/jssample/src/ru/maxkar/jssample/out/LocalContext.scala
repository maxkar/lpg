package ru.maxkar.jssample.out

import ru.maxkar.scoping.simple._

import ru.maxkar.jssample.msg.HostTrace
import ru.maxkar.backend.js.model._

/** Local compilation context. */
private[out] final class LocalContext(
    val variables : SymbolScope,
    val labels : Scope[String, Symbol],
    val trace : HostTrace,
    host : java.io.File,
    val funcImpl : (Symbol, FunctionBody) ⇒ Unit) {

  val exprs = new ExprComp(host, trace, variables)
}
