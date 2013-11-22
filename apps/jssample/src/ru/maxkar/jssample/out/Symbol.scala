package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.backend.js.model._
import ru.maxkar.scoping.simple._

/**
 * Symbol, may define symbol in a source code or a reference to
 * another object.
 */
final class Symbol(val declaration : ModuleHost) {
  def resolve() : LeftValue = Model.variable(this)
}
