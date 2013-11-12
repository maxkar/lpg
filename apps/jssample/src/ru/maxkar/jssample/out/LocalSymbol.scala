package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._
import ru.maxkar.backend.js.model._
import ru.maxkar.scoping.simple._

/** Local symbol, visible to some scope. */
private[out] final class LocalSymbol(val declaration : ModuleHost)
    extends Symbol {
  def resolve() : Expression = Model.variable(this)
}
