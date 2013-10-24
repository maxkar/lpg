package ru.maxkar.lispy

/** S-expression type. Each type may be either a leaf
 * of specific type or list of children expressions.
 * Unlike commmon (lisp) s-expressions, these expressions
 * have general attributes attached to each expression.
 */
abstract sealed class SExpression[L]


/**
 * Leaf expression.
 * @param value leaf value.
 * @param atts additional expression attributes.
 */
final case class SLeaf[L](
  value : L, atts : Attributes) extends SExpression[L]


/**
 * S-expression list node.
 * @param items s-list content.
 * @param atts list attributes.
 */
final case class SList[L](
    items : Seq[SExpression[L]],
    atts : Attributes) extends SExpression[L] {
}
