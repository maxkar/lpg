package ru.maxkar.scoping.simple

/**
 * Name scope. Provides a mapping from key to set of
 * names. Usually a single "definition" scope contains
 * only one name, but effective scope (composition of
 * other scopes) may contain several items.
 */
trait Scope[-K, V] {
  /** Finds all suitable definitions for the given key. */
  def lookup(key : K) : Set[V]
}


/** Scope utilities. */
final object Scope {
  /** Creates a new scope, which returns results from
   * all parent scopes.
   */
  def join[K, V](items : Scope[K, V]*) : Scope[K, V] =
    new Scope[K, V] {
      def lookup(key : K) : Set[V] = {
        def sum(s : Set[V], sc : Scope[K, V]) : Set[V] =
          s ++ sc.lookup(key)
        items.foldLeft(Set.empty[V])(sum)
      }
    }

  /** Returns a chained lookup. Prefers to return value
   * from a child scope unless it contain no value for the
   * key.
   */
  def chain[K, V](parent : Scope[K, V], child : Scope[K, V]) : Scope[K, V] =
    new Scope[K, V] {
      def lookup(key : K) : Set[V] = {
        val g1 = child.lookup(key)
        if (g1.isEmpty)
          parent.lookup(key)
        else
          g1
      }
    }
}
