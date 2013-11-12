package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._
import ru.maxkar.backend.js.model._
import ru.maxkar.scoping.simple._

/** Global symbol, visible to other modules. */
private[out] final class GlobalSymbol(val declaration : ModuleHost, name : String)
    extends Symbol {
  def resolve() : Expression = Model.global(name)
}

