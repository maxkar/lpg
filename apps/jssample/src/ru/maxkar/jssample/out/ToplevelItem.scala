package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.backend.js.model._
import ru.maxkar.scoping.simple._

/** Item which can be output into the context. */
trait ToplevelItem {
  val declarationHost : ModuleHost
  def resolve(ctx : Scope[String, ToplevelItem]) : Expression
}
