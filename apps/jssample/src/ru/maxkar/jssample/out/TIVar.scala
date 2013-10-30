package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._

/** Top-level variable. */
private[out] final class TIVar(val declarationHost : ModuleHost, expr : Option[SExpression[BaseItem]])
  extends ToplevelItem {
}
