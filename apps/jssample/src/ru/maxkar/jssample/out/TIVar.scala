package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._
import ru.maxkar.backend.js.model._
import ru.maxkar.scoping.simple._

/** Top-level variable. */
private[out] final class TIVar(val declarationHost : ModuleHost, name : String)
    extends ToplevelItem {
  def resolve(ctx : Scope[String, ToplevelItem]) : Expression = Model.global(name)
}
