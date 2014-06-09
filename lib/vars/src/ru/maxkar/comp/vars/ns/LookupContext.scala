package ru.maxkar.comp.vars.ns

import ru.maxkar.collection.LazySeq

/**
 * Namespace lookup context. Namespace contexts can be
 * either explicit (variable from a specified context) or
 * implicit (variable/name is resolved against different
 * context).
 */
abstract class LookupContext[T] private(private[ns] val _scopes : LazySeq[T]) {
  /** Lookup scopes defined for this context. */
  val scopes : Iterable[T] = _scopes

  /** Nest context into other context. */
  private[ns] def inside(other : LookupContext[T]) : LookupContext[T]
}


/**
 * Namespace lookup context copmanion.
 */
object LookupContext {

  /** Nestable context. */
  private class NestableContext[T](items : LazySeq[T])
      extends LookupContext[T](items) {
    override private[ns] def inside(other : LookupContext[T]) : LookupContext[T] =
      new NestableContext[T](items ++ other._scopes)
  }

  /**
   * Creates explicit context. This context will be not modified
   * after appending other contexts.
   */
  def explicit[T](scope : T) : LookupContext[T] =
    new LookupContext[T](LazySeq(scope)) {
      override private[ns] def inside(other : LookupContext[T]) : LookupContext[T] =
        this
    }

  /**
   * Nestable context. This context can attach parents to itself thus
   * forming a new lookup chain.
   */
  def nestable[T](scope : T) : LookupContext[T] =
    new NestableContext[T](LazySeq(scope))
}
