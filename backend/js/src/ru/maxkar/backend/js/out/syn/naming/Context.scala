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
   * Delegates resolution to target expression.
   * Usefull for high-level functions (map).
   */
  def resolveExpr[E](expression : NamingExpression[V, L, E]) : E =
    expression.resolve(this)

  /** Resolvel list of expressions. */
  def resolveExprs[E](expressions : Seq[NamingExpression[V, L, E]]) : Seq[E] =
    expressions map resolveExpr

  /**
   * Delegates resolution to target statement.
   * Usefull for high-level functions (map).
   */
  def resolveStmt[S](statement : NamingStatement[V, L, S]) : S =
    statement.resolve(this)

  /** Resolves list of statements. */
  def resolveStmts[S](statements : Seq[NamingStatement[V, L, S]]) : Seq[S] =
    statements map resolveStmt

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

  /**
   * Creates a subcontext for the catch clause. Unlike sub, this
   * subcontext shares labels with this base context.
   */
  def subExn(variable : V) : Context[V, L] = {
    val subIdGen = idGen.slice
    val subIds = idMap + ((variable, subIdGen.next()))

    new Context(subIdGen, labelGen, subIds, labelMap)
  }

}
