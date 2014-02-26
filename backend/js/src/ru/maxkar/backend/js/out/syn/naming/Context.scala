package ru.maxkar.backend.js.out.syn.naming

/** Naming context. Names generator and mapping form
 * identifiers to actual names.
 * @param V variable name type.
 * @param L label name type.
 */
private[naming] abstract class Context[V, L] {
  /** Fetches a variable name. */
  def variable(name : V) : String

  /** Resolves a label. */
  def label(name : L) : String

  /**
   * Creates a subcontext.
   * @param vars variables defined in the scope.
   * @param labels labels defined in the scope.
   */
  def sub(vars : Set[V], labels : Set[L]) : Context[V, L]

}
