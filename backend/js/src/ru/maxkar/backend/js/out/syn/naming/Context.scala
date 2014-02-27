package ru.maxkar.backend.js.out.syn.naming

/** Naming context. Names generator and mapping form
 * identifiers to actual names.
 * @param V variable name type.
 * @param L label name type.
 */
private[naming] final class Context[V, L](
    idGen : CachingIdGenerator,
    labelGen : CachingIdGenerator,
    idMap : Map[V, String],
    labelMap : Map[L, String]
  ){

  /** Fetches a variable name. */
  def variable(name : V) : String =
    idMap(name)

  /** Resolves a label. */
  def label(name : L) : String =
    labelMap(name)

  /**
   * Creates a subcontext.
   * @param vars variables defined in the scope.
   * @param labels labels defined in the scope.
   */
  def sub(vars : Set[V], labels : Set[L]) : Context[V, L] = {
    val subIdGen = idGen.slice
    val subLabelGen = labelGen.slice
    val subIds = idMap ++ vars.iterator.map(x ⇒ (x, subIdGen.next()))
    val subLabels = labels.iterator.map(x ⇒ (x, subLabelGen.next())).toMap

    new Context(subIdGen, labelGen, subIds, subLabels)
  }

}
