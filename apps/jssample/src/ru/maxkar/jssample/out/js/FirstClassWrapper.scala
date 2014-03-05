package ru.maxkar.jssample.out.js

/**
 * Wrapper for a first-class expression.
 * @param S statement type.
 * @param id expression (top-level) identifier.
 * @param wrapArgs wrapper arguments.
 * @param impl operator implementation.
 */
private[js] final class FirstClassWrapper[S](
  val id : AnyRef,
  val wrapArgs : Seq[AnyRef],
  val impl : Seq[S])
