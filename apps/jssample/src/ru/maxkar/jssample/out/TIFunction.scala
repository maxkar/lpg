package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._

/** Top-level Function. */
private[out] final class TIFunction(
    val declarationHost : ModuleHost,
    args : Seq[SExpression[BaseItem]],
    body : Seq[SExpression[BaseItem]])
  extends ToplevelItem {
}

