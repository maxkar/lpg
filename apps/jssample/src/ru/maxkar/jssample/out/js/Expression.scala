package ru.maxkar.jssample.out.js

/**
 * Javascript expression value.
 * @param base base expression.
 * @param constValue base constant value.
 * @param invokeHandler invokation handler for this expression.
 * @param firstClassOperators set of operators used as a f
 *   first-class constructs.
 * @param E base expression type.
 * @param S base statement type.
 */
final class Expression[E, S] private[js](
    private[js] val base : E,
    private[js] val constValue : Option[String],
    private[js] val invokeHandler :
      (Expression[E, S], Seq[Expression[E, S]]) ⇒ Expression[E, S],
    private[js] val firstClassOperators : Set[FirstClassWrapper[S]]
  ) {
}

