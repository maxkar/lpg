package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._
import ru.maxkar.backend.js.model._
import ru.maxkar.scoping.simple._

/** Top-level Function. */
private[out] final class TIFunction(
      val declarationHost : ModuleHost,
      name : String,
      args : Seq[SExpression[BaseItem]],
      body : Seq[SExpression[BaseItem]])
    extends ToplevelItem {
  def resolve(ctx : Scope[String, ToplevelItem]) : Expression = Model.global(name)
}

