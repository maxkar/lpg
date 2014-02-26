package ru.maxkar.backend.js.out.syn.naming

/**
 * Expression for the naming syntax.
 * @param E type of the underlying expression.
 * @param V type of the variable.
 * @param L label type.
 */
abstract sealed class NamingExpression[E, V, L] {
  /**
   * Set of variables used but not defined inside this expression.
   */
  val usedVariables : Set[V]

  /** Resolves this expression in the context. */
  def resolve(context : Context[V, L]) : E
}

/** Instant expression. It is an expression with no unresolved variables. */
private[naming] final case class InstantExpression[E, V, L](value : E)
    extends NamingExpression[E, V, L] {

  override val usedVariables : Set[V] = Set.empty

  override def resolve(context : Context[V, L]) : E = value
}

/** Unresolved expression. Will be resolved later. */
private[naming] final class UnresolvedExpression[E, V, L](
      override val usedVariables : Set[V], resolver : Context[V, L] ⇒ E)
    extends NamingExpression[E, V, L] {

  override def resolve(context : Context[V, L]) : E = resolver(context)
}

/** Pattern unmatcher for the naming expression. */
private[naming] object InstE {
  def unapply[E, V, L](item : NamingExpression[E, V, L]) : Option[E] =
    item match {
      case InstantExpression(v) ⇒ Some(v)
      case _ ⇒ None
    }
}

/** Sequence unpacker. */
private[naming] object InstES {
  def unapply[E, V, L](items : Seq[NamingExpression[E, V, L]]) :
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
