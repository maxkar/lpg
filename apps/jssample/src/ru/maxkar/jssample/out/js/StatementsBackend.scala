package ru.maxkar.jssample.out.js

import ru.maxkar.backend.js.out.syn.Syntax

final class StatementsBackend[BE, BS](base : Syntax[AnyRef, AnyRef, BE, BS])
    extends ru.maxkar.jssample.out.StatementsBackend[Expression[BE, BS], Statement[BE, BS]] {
  /** Expression type. */
  type E = Expression[BE, BS]
  /** Statement type. */
  type S = Statement[BE, BS]


  override def exprStmt(expr : E) : S =
    new Statement(base.exprStatement(expr.base), expr.firstClassOperators)


  override def declVars(names : Seq[AnyRef]) : S =
    new Statement(base.defVars(names.map(x ⇒ (x, None))), Set.empty)


  override def defun(name : AnyRef, args : Seq[AnyRef], body : Seq[S]) : S =
    new Statement(base.functionStmt(
        Some(name), args, unpack(body)),
      aggSpecS(body))


  override def retVal(value : E) : S =
    new Statement(base.ret(value.base), value.firstClassOperators)


  override val retNone : S = new Statement(base.retNone, Set.empty)


  override def whileDo(cond : E, stmts : Seq[S]) : S =
    new Statement(base.whileDo(cond.base, unpack(stmts)),
      cond.firstClassOperators ++ aggSpecS(stmts))


  override def ifDo(cond : E, onTrue : Seq[S], onFalse : Seq[S]) : S =
    new Statement(
      base.ifStatement(cond.base, unpack(onTrue), unpack(onFalse)),
      cond.firstClassOperators ++ aggSpecS(onTrue) ++ aggSpecS(onFalse))


  override def forKey(iterator : AnyRef, collection : E, stmts : Seq[S]) : S =
    new Statement(
      base.forVariableIn(iterator, collection.base, unpack(stmts)),
      collection.firstClassOperators ++ aggSpecS(stmts))


  override def catchExn(exn : AnyRef, body : Seq[S], handler : Seq[S]) : S =
    new Statement(
      base.tryCatch(unpack(body), Some((exn, unpack(handler))), Seq.empty),
      aggSpecS(body) ++ aggSpecS(handler))


  override def valueMatch(expr : E, conds : Seq[(Seq[E], Seq[S])], dflt : Seq[S]) : S =
    new Statement(
      base.chooseValue(
        expr.base,
        conds.map(x ⇒
          if (x._1.isEmpty)
            Seq.empty
          else
            (x._1.dropRight(1).map(x ⇒ (x.base, Seq.empty)) :+
             ((x._1.last.base, unpack(x._2))))
         ).flatten,
        unpack(dflt)),
      expr.firstClassOperators ++
        aggSpecE(conds.map(_._1).flatten) ++
        aggSpecS(conds.map(_._2).flatten) ++
        aggSpecS(dflt))


  /** Unpacks statemets. */
  private def unpack(stmts : Seq[S]) : Seq[BS] =
    stmts.map(_.base)

  /** Aggregates specials from expressions. */
  private def aggSpecE(args : Seq[E]) : Set[FirstClassWrapper[BS]] =
    args.foldLeft(Set.empty[FirstClassWrapper[BS]])(
      (s, a) ⇒ s ++ a.firstClassOperators)

  /** Aggregates specials from statements. */
  private def aggSpecS(args : Seq[S]) : Set[FirstClassWrapper[BS]] =
    args.foldLeft(Set.empty[FirstClassWrapper[BS]])(
      (s, a) ⇒ s ++ a.firstClassOperators)
}
