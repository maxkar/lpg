package ru.maxkar.backend.js.out.syn.naming

/**
 * Statement for the naming generator.
 * @param usedVariables
 *   all variables referenced in this statement.
 * @param definedVariables
 *  variables defined in this statement and visible to current scope
 *  (function).
 * @param usedLabels referenced labels.
 * @param resolver statement resolver.
 * @param V value type.
 * @param L label type.
 * @param S base statement type.
 */
final class NamingStatement[V, L, S](
  val usedVariables : Set[V],
  val definedVariables : Set[V],
  val usedLabels : Set[L],
  resolver : Context[V, L] ⇒ S
) {

  /** Resolves an expression in the context. */
  def resolve(context : Context[V, L]) : S = resolver(context)
}
