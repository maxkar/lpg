package ru.maxkar.jssample.out

/** Basic definition for the statements backend. */
trait StatementsBackend[E, S] {
  /** Creates statement from the expression. */
  def exprStmt(expr : E) : S

  /** Declares variables. */
  def declVars(names : Seq[AnyRef]) : S

  /** Defines a named function. */
  def defun(name : AnyRef, args : Seq[AnyRef], body : Seq[S]) : S

  /** Returns a value. */
  def retVal(value : E) : S

  /** Returns without a value. */
  val retNone : S

  /** Performs a "while ... do" iterations. */
  def whileDo(cond : E, stmts : Seq[S]) : S

  /** Conditionally performs operations. */
  def ifDo(cond : E, onTrue : Seq[S], onFalse : Seq[S]) : S

  /** Iterates collection on keys. */
  def forKey(iterator : AnyRef, collection : E, stmts : Seq[S]) : S

  /** Catches an exception. */
  def catchExn(exn : AnyRef, body : Seq[S], handler : Seq[S]) : S

  /** Performs a choice based on a value. */
  def valueMatch(expr : E, conds : Seq[(Seq[E], Seq[S])], dflt : Seq[S]) : S
}

