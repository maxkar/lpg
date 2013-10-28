package ru.maxkar.backend.js.out

import scala.collection.mutable.HashMap


/** Identifiers scope. Provides mapping from
 * abstract id codes to it's names.
 * @param parent parent scope.
 * @param items items in this scope.
 * @node base node for this scope.
 */
private[out] final class IdScope(
    parent : AnyRef ⇒ String,
    items : Iterable[AnyRef],
    private var base : IdGenNode) {

  /** Local map from id's to it's values. */
  private val localMap = new HashMap[AnyRef, String]

  /* Initialize all own id's. */
  items.foreach(
    x ⇒ localMap.getOrElseUpdate(x, {
        val r = base.name
        base = base.next
        r
      }))

  /** Lookups an identifier. */
  def lookup(key : AnyRef) : String =
    localMap.getOrElseUpdate(key, parent(key))

  /** Creates a subcontext. */
  def sub(keys : Iterable[AnyRef]) : IdScope =
    new IdScope(lookup, keys, base)
}


/** Identifier scope utilities. */
private[out] final object IdScope {
  /** Default id resolver. */
  private def defaultResolve(id : AnyRef) : String =
    throw new BadVariableException(id)

  /** Default label resolver. */
  private def defaultLabel(id : AnyRef) : String =
    throw new BadLabelException(id)

  /** Creates a new (root) context. */
  def default(blacklist : String ⇒  Boolean) : IdScope =
    new IdScope(defaultResolve, Seq.empty, IdGenNode.default(blacklist))

  /** Creates a new label resolver. */
  def labels(blacklist : String ⇒  Boolean) : IdScope =
    new IdScope(defaultLabel, Seq.empty, IdGenNode.default(blacklist))
}
