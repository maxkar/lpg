package ru.maxkar.jssample.out.js

/**
 * Statement implementation.
 * @param E underlying expression type.
 * @param S underlying statement type.
 */
final class Statement[E, S](
  private[js] val base : S,
  private[js] val firstClassOperators : Set[FirstClassWrapper[S]]
)
