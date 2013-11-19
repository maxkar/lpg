package ru.maxkar.jssample.out

import ru.maxkar.scoping.simple._

import ru.maxkar.jssample.msg.HostTrace

/** Local compilation context. */
private[out] final class LocalContext(
    val variables : SymbolScope,
    val labels : Scope[String, Symbol],
    val trace : HostTrace,
    host : java.io.File) {

  val exprs = new ExprComp(host, trace, variables)
}
