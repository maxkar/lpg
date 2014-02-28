package ru.maxkar.backend.js.out.syn.naming

/**
 * Expression for the naming syntax.
 * @param V type of the variable.
 * @param L label type.
 * @param E type of the underlying expression.
 */
abstract sealed class NamingExpression[V, L, E] {
  /**
   * Set of variables used inside this expression.
   */
  val usedVariables : Set[V]

  /** Resolves this expression in the target context. */
  def resolve(context : Context[V, L]) : E
}

/** Instant expression. It is an expression with no unresolved variables. */
private[naming] final case class InstantExpression[V, L, E](value : E)
    extends NamingExpression[V, L, E] {

  override val usedVariables : Set[V] = Set.empty

  override def resolve(context : Context[V, L]) : E = value
}

/** Unresolved expression. Will be resolved later. */
private[naming] final class UnresolvedExpression[V, L, E](
      override val usedVariables : Set[V], resolver : Context[V, L] ⇒ E)
    extends NamingExpression[V, L, E] {

  override def resolve(context : Context[V, L]) : E = resolver(context)
}

/** Pattern unmatcher for the naming expression. */
private[naming] object InstE {
  def unapply[V, L, E](item : NamingExpression[V, L, E]) : Option[E] =
    item match {
      case InstantExpression(v) ⇒ Some(v)
      case _ ⇒ None
    }
}

/** Sequence unpacker. */
private[naming] object InstES {
  def unapply[V, L, E](items : Seq[NamingExpression[V, L, E]]) :
      Option[Seq[E]] = {
    val res = new scala.collection.mutable.ArrayBuffer[E](items.size)

    val itr = items.iterator
    while (itr.hasNext)
      itr.next match {
        case InstE(v) ⇒ res += v
        case _ ⇒ return None
      }

    Some(res)
 }
}
