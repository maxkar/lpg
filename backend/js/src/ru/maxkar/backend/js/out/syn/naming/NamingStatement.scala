package ru.maxkar.backend.js.out.syn.naming

/**
 * Statement for the naming generator.
 * @param referencedVariables variables referenced in this statement.
 * @param definedVariables variables defined in this statement.
 * @param referencedLabels referenced labels.
 * @param resolver statement resolver.
 * @param V value type.
 * @param L label type.
 * @param S base statement type.
 */
final class NamingStatement[V, L, S](
  val referencedVariables : Set[V],
  val definedVariables : Set[V],
  val referencedLabels : Set[L],
  resolver : Context[V, L] ⇒ S
) {

  /** Resolves an expression in the context. */
  def resolve(context : Context[V, L]) : S = resolver(context)
}
