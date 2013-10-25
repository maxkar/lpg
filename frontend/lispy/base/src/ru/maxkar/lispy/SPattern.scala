package ru.maxkar.lispy

/** Patterns to simplify S-expression pattern matching. */
final object SPattern {
  /** Matches S-list starting with identifier.
   * Chops head identifier from the head.
   * Returns identifier, it's attributes and rest of the list.
   */
  final object IdList {
    def unapply(expr : SExpression[BaseItem]) :
        Option[(String, Attributes, Seq[SExpression[BaseItem]])] = {
      expr match {
        case SList(Seq(SLeaf(BaseId(n), a), tl@_*), _) ⇒ Some((n, a, tl))
        case _ ⇒ None
      }
    }
  }


  /** Matcher for the Identifier leaf. */
  final object IdLeaf {
    def unapply(expr : SExpression[BaseItem]) :
        Option[(String, Attributes)] = {
      expr match {
        case SLeaf(BaseId(n), a) ⇒ Some((n, a))
        case _ ⇒ None
      }
    }
  }
}
