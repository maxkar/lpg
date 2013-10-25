package ru.maxkar.lispy

/** S-expression type. Each type may be either a leaf
 * of specific type or list of children expressions.
 * Unlike commmon (lisp) s-expressions, these expressions
 * have general attributes attached to each expression.
 */
abstract sealed class SExpression[L >: Null] {
  /** Returns value from the leaf. May return null. */
  def unLeaf() : L
  /** Returns a leading leaf value if this node is a
   * sequence and first value is leaf.
   * Returns null otherwise.
   */
  def unHeadLeaf() : L

  /** Returns a first child. If expression is leaf or empty, returns null. */
  def head() : SExpression[L]

  /** Returns a tail of the expression.
   * If expression is leaf or empty, returns null.
   */
  def tail() : Seq[SExpression[L]]

  /** Expression attributs. */
  val atts : Attributes
}


/**
 * Leaf expression.
 * @param value leaf value.
 * @param atts additional expression attributes.
 */
final case class SLeaf[L >: Null](
    value : L, atts : Attributes) extends SExpression[L] {

  def head() :  SExpression[L] = null

  def tail() : Seq[SExpression[L]] = null

  def unLeaf() : L = value

  def unHeadLeaf() : L = null
}


/**
 * S-expression list node.
 * @param items s-list content.
 * @param atts list attributes.
 */
final case class SList[L >: Null](
    items : Seq[SExpression[L]],
    atts : Attributes) extends SExpression[L] {

  def head() : SExpression[L] =
    if (items.isEmpty)
      null
    else
      items.head

  def tail() : Seq[SExpression[L]] =
    if (items.isEmpty)
      null
    else
      items.tail

  def unLeaf() : L = null

  def unHeadLeaf() : L =
    if (items.isEmpty)
      null
    else
      items.head.unLeaf
}
