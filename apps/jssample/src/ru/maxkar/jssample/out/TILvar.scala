package ru.maxkar.jssample.out


import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._
import ru.maxkar.backend.js.model._
import ru.maxkar.scoping.simple._

/** Local variable. */
private[out] final class TILvar(val declarationHost : ModuleHost)
    extends ToplevelItem {
  def resolve(ctx : Scope[String, ToplevelItem]) : Expression = Model.variable(this)
}
