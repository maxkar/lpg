package ru.maxkar.backend.js.out.syn.naming
import ru.maxkar.backend.js.out.syn.Syntax

import java.math.BigInteger
import java.math.BigDecimal

/**
 * Syntax implementation for autonaming genetator.
 * Generates string names for the elements of the input.
 * This allows to use non-string identifiers for the variables (identity ids for example).
 */
final class NamingSyntax[V, L, BE, BS](base : Syntax[String, String, BE, BS])
    extends Syntax[V, L, NamingExpression[BE, V, L], NamingStatement[V, L, BS]] {

  import NamingSyntax._

  /** Expression type synonim. */
  private type E = NamingExpression[BE, V, L]
  /** Statement type synonim. */
  private type S = NamingStatement[V, L, BS]
  /** Context type alias. */
  private type C = Context[V, L]


  /* New public functions. */

  /**
   * General converter form naming to base.
   * Treats this sequence as a single program. This means that statements
   * may refer to variables defined in other statements and this will be valid.
   * This generator will create unique names for all used variables which are
   * absent in <code>predefinedNames</code>.
   * @param predefinedNames list of predefined names (platform names,
   *   exported names, etc...
   * @param items items to convert.
   */
  def compile(predefinedNames : Map[V, String], items : Seq[S]) : Seq[BS] = {
    val (uv, dv, dl) = analyze(items)
    val sysnames = predefinedNames.values.toSet
    val nameGen = idGen(sysnames)
    val labelGen = idGen(Set.empty)
    val rootCtx = new Context[V, L](
      nameGen, labelGen, predefinedNames, Map.empty)
    val ctx = rootCtx.sub(uv ++ dv, dl)

    items.map(x ⇒  x.resolve(ctx))
  }



  /* Trait implementation. */
  override val exprUndefined = iexpr(base.exprUndefined)

  override val exprNull = iexpr(base.exprNull)

  override val exprArguments = iexpr(base.exprArguments)

  override def literal(value : String) : E =
    iexpr(base.literal(value))

  override def literal(value : BigInteger) : E =
    iexpr(base.literal(value))

  override def literal(value : BigDecimal, exp : BigInteger) : E =
    iexpr(base.literal(value, exp))

  override def literal(value : Boolean) : E =
    iexpr(base.literal(value))

  override def arrayLiteral(items : Seq[E]) : E =
    items match {
      case InstES(sub) ⇒ iexpr(base.arrayLiteral(sub))
      case _ ⇒
        dexpr(
          items.foldLeft(Set.empty[V])((s, i) ⇒ s ++ i.usedVariables),
          c ⇒ base.arrayLiteral(items map (i ⇒ i.resolve(c))))
    }

  override def objectLiteral(items : Seq[(String, E)]) : E =
    items.map(_._2) match {
      case InstES(sub) ⇒
        iexpr(base.objectLiteral(items.map(_._1).zip(sub)))
      case _ ⇒
        dexpr(
          items.foldLeft(Set.empty[V])((s, i) ⇒ s ++ i._2.usedVariables),
          c ⇒ base.objectLiteral(items map (i ⇒ (i._1, i._2.resolve(c))))
        )
    }

  override def variable(v : V) : E =
    dexpr(Set(v), c ⇒ base.variable(c.variable(v)))

  override def functionExpr(name : Option[V], args : Seq[V], body : Seq[S]) : E = {
    var (allReferenced, innerDefined, allLabels) =
      analyze(body)
    val allDefined = innerDefined ++ name ++ args
    dexpr(
      allReferenced -- allDefined,
      c ⇒ {
        val subctx = c.sub(allDefined, allLabels)
        base.functionExpr(
          name.map(subctx.variable),
          args.map(subctx.variable),
          body.map(stmt ⇒ stmt.resolve(subctx)))
      })
  }

  override def knownMember(value : E, name : String) : E =
    unary(value, base.knownMember(_, name))

  override def dynamicMember(value : E, member : E) : E =
    binary(value, member, base.dynamicMember)

  override def create(host : E, args : Seq[E]) : E =
    (host, args) match {
      case (InstE(h), InstES(a)) ⇒
        iexpr(base.create(h, a))
      case _ ⇒ dexpr(
        args.foldLeft(host.usedVariables)(
          (s, e) ⇒ s ++ e.usedVariables),
        ctx ⇒ base.create(
          host.resolve(ctx),
          args.map(a ⇒ a.resolve(ctx)))
        )
    }

  override def call(host : E, args : Seq[E]) : E =
    (host, args) match {
      case (InstE(h), InstES(a)) ⇒
        iexpr(base.call(h, a))
      case _ ⇒ dexpr(
        args.foldLeft(host.usedVariables)(
          (s, e) ⇒ s ++ e.usedVariables),
        ctx ⇒ base.call(
          host.resolve(ctx),
          args.map(a ⇒ a.resolve(ctx)))
        )
    }

  override def prefixInc(value : E) : E =
    unary(value, base.prefixInc)

  override def prefixDec(value : E) : E =
    unary(value, base.prefixDec)

  override def postfixInc(value : E) : E =
    unary(value, base.postfixInc)

  override def postfixDec(value : E) : E =
    unary(value, base.postfixDec)

  override def bitNot(value : E) : E =
    unary(value, base.bitNot)

  override def boolNot(value : E) : E =
    unary(value, base.boolNot)

  override def neg(value : E) : E =
    unary(value, base.neg)

  override def typeOf(value : E) : E =
    unary(value, base.typeOf)

  override def voidOf(value : E) : E =
    unary(value, base.voidOf)

  override def delete(value : E) : E =
    unary(value, base.delete)

  override def mul(value1 : E, value2 : E) : E =
    binary(value1, value2, base.mul)

  override def div(value1 : E, value2 : E) : E =
    binary(value1, value2, base.div)

  override def rem(value1 : E, value2 : E) : E =
    binary(value1, value2, base.rem)

  override def add(value1 : E, value2 : E) : E =
    binary(value1, value2, base.add)

  override def sub(value1 : E, value2 : E) : E =
    binary(value1, value2, base.sub)

  override def shl(value1 : E, value2 : E) : E =
    binary(value1, value2, base.shl)

  override def sshr(value1 : E, value2 : E) : E =
    binary(value1, value2, base.sshr)

  override def ushr(value1 : E, value2 : E) : E =
    binary(value1, value2, base.ushr)

  override def less(value1 : E, value2 : E) : E =
    binary(value1, value2, base.less)

  override def lessEq(value1 : E, value2 : E) : E =
    binary(value1, value2, base.lessEq)

  override def greater(value1 : E, value2 : E) : E =
    binary(value1, value2, base.greater)

  override def greaterEq(value1 : E, value2 : E) : E =
    binary(value1, value2, base.greaterEq)

  override def isIn(value1 : E, value2 : E) : E =
    binary(value1, value2, base.isIn)

  override def isInstanceOf(value1 : E, value2 : E) : E =
    binary(value1, value2, base.isInstanceOf)

  override def equals(value1 : E, value2 : E) : E =
    binary(value1, value2, base.equals)

  override def notEquals(value1 : E, value2 : E) : E =
    binary(value1, value2, base.notEquals)

  override def strictEquals(value1 : E, value2 : E) : E =
    binary(value1, value2, base.strictEquals)

  override def strictNotEquals(value1 : E, value2 : E) : E =
    binary(value1, value2, base.strictNotEquals)

  override def bitAnd(value1 : E, value2 : E) : E =
    binary(value1, value2, base.bitAnd)

  override def bitXor(value1 : E, value2 : E) : E =
    binary(value1, value2, base.bitXor)

  override def bitOr(value1 : E, value2 : E) : E =
    binary(value1, value2, base.bitOr)

  override def boolAnd(value1 : E, value2 : E) : E =
    binary(value1, value2, base.boolAnd)

  override def boolOr(value1 : E, value2 : E) : E =
    binary(value1, value2, base.boolOr)

  override def condExpr(cond : E, onTrue : E, onFalse : E) : E =
    (cond, onTrue, onFalse) match {
      case (InstE(c), InstE(t), InstE(f)) ⇒
        iexpr(base.condExpr(c, t, f))
      case _ ⇒ dexpr(
        cond.usedVariables ++ onTrue.usedVariables ++ onFalse.usedVariables,
        ctx ⇒ base.condExpr(
          cond.resolve(ctx), onTrue.resolve(ctx), onFalse.resolve(ctx)))
    }

  override def assign(value1 : E, value2 : E) : E =
    binary(value1, value2, base.assign)

  override def inplaceAdd(value1 : E, value2 : E) : E =
    binary(value1, value2, base.inplaceAdd)

  override def inplaceSub(value1 : E, value2 : E) : E =
    binary(value1, value2, base.inplaceSub)

  override def inplaceMul(value1 : E, value2 : E) : E =
    binary(value1, value2, base.inplaceMul)

  override def inplaceDiv(value1 : E, value2 : E) : E =
    binary(value1, value2, base.inplaceDiv)

  override def inplaceRem(value1 : E, value2 : E) : E =
    binary(value1, value2, base.inplaceRem)

  override def inplaceShl(value1 : E, value2 : E) : E =
    binary(value1, value2, base.inplaceShl)

  override def inplaceSshr(value1 : E, value2 : E) : E =
    binary(value1, value2, base.inplaceSshr)

  override def inplaceUshr(value1 : E, value2 : E) : E =
    binary(value1, value2, base.inplaceUshr)

  override def inplaceBitAnd(value1 : E, value2 : E) : E =
    binary(value1, value2, base.inplaceBitAnd)

  override def inplaceBitXor(value1 : E, value2 : E) : E =
    binary(value1, value2, base.inplaceBitXor)

  override def inplaceBitOr(value1 : E, value2 : E) : E =
    binary(value1, value2, base.inplaceBitOr)

  override def seqExpr(first : E, tail : Seq[E]) : E =
    (first, tail) match {
      case (InstE(f), InstES(t)) ⇒
        iexpr(base.seqExpr(f, t))
      case _ ⇒ dexpr(
        tail.foldLeft(first.usedVariables)(
          (s, v) ⇒ s ++ v.usedVariables),
        ctx ⇒ base.seqExpr(
          first.resolve(ctx),
          tail.map(x ⇒ x.resolve(ctx)))
        )
    }

  override def exprStatement(expr : E) : S =
    expr match {
      case InstE(e) ⇒ stmt(
          Set.empty, Set.empty, Set.empty,
          ctx ⇒ base.exprStatement(e))
      case _ ⇒ stmt(
          expr.usedVariables, Set.empty, Set.empty,
          ctx ⇒ base.exprStatement(expr.resolve(ctx)))
    }

  override val breakOuter : S = stmt(
    Set.empty, Set.empty, Set.empty,
    ctx ⇒  base.breakOuter)

  override def breakLabeled(label : L) : S =
    stmt(Set.empty, Set.empty, Set(label),
      ctx ⇒ base.breakLabeled(ctx.label(label)))

  override val continueOuter : S = stmt(
    Set.empty, Set.empty, Set.empty,
    ctx ⇒ base.continueOuter)

  override def continueLabeled(label : L) : S =
    stmt(Set.empty, Set.empty, Set(label),
      ctx ⇒ base.continueLabeled(ctx.label(label)))

  override def doWhile(body : Seq[S] ,cond : E) : S = {
    val (uv, dv, dl) = analyze(body)
    stmt(
      cond.usedVariables ++ uv, dv, dl,
      ctx ⇒ base.doWhile(
        body.map(b ⇒ b.resolve(ctx)),
        cond.resolve(ctx)))
  }

  override def forVars(
        variables : Seq[(V, Option[E])],
        condition : Option[E],
        finalExpression : Option[E],
        body : Seq[S])
      : S = {
    val (uv, dv, dl) = analyze(body)
    val allUsed =
      variables.foldLeft(uv)(
        (s, x) ⇒ s ++ varsOfOpt(x._2)) ++
      varsOfOpt(condition) ++ varsOfOpt(finalExpression)
    val allDef = dv ++ variables.map(_._1)

    stmt(
      allUsed, allDef, dl,
      ctx ⇒ base.forVars(
        variables.map(v ⇒
          (ctx.variable(v._1), v._2.map(e ⇒ e.resolve(ctx)))),
        condition.map(e ⇒ e.resolve(ctx)),
        finalExpression.map(e ⇒ e.resolve(ctx)),
        body.map(s ⇒ s.resolve(ctx)))
    )
  }

  override def forExisting(
        init : Option[E],
        condition : Option[E],
        finalExpression : Option[E],
        body : Seq[S])
      : S = {
    val (uv, dv, dl) = analyze(body)
    val allUsed = uv ++ varsOfOpt(init) ++
      varsOfOpt(condition) ++ varsOfOpt(finalExpression)

    stmt(
      allUsed, dv, dl,
      ctx ⇒ base.forExisting(
        init.map(e ⇒ e.resolve(ctx)),
        condition.map(e ⇒ e.resolve(ctx)),
        finalExpression.map(e ⇒ e.resolve(ctx)),
        body.map(s ⇒ s.resolve(ctx)))
    )
  }

  override def forVariableIn(variable : V, collection : E, body : Seq[S]) : S = {
    val (uv, dv, dl) = analyze(body)
    val allUsed = uv ++ collection.usedVariables

    stmt(
      allUsed, dv + variable, dl,
      ctx ⇒ base.forVariableIn(
        ctx.variable(variable),
        collection.resolve(ctx),
        body.map(x ⇒ x.resolve(ctx)))
    )
  }

  override def forExistingIn(iterator : E, collection : E, body : Seq[S]) : S = {
    val (uv, dv, dl) = analyze(body)
    val allUsed = uv ++ collection.usedVariables ++ iterator.usedVariables

    stmt(
      allUsed, dv, dl,
      ctx ⇒ base.forExistingIn(
        iterator.resolve(ctx),
        collection.resolve(ctx),
        body.map(x ⇒ x.resolve(ctx)))
    )
  }

  override def functionStmt(name : Option[V], args : Seq[V], body : Seq[S]) : S = {
    val (allReferenced, innerDefined, allLabels) =
      analyze(body)
    val allDefined = innerDefined ++ name ++ args
    stmt(
      allReferenced -- allDefined, name.toSet, Set.empty,
      c ⇒ {
        val subctx = c.sub(allDefined, allLabels)
        base.functionStmt(
          name.map(subctx.variable),
          args.map(subctx.variable),
          body.map(stmt ⇒ stmt.resolve(subctx)))
      })
  }

  override def ifStatement(condition : E, onTrue : Seq[S], onFalse : Seq[S]) : S = {
    val (tuv, tdv, tdl) = analyze(onTrue)
    val (fuv, fdv, fdl) = analyze(onFalse)
    val allUsed = tuv ++ fuv ++ condition.usedVariables

    stmt(
      allUsed, tdv ++ fdv, tdl ++ fdl,
      ctx ⇒ base.ifStatement(
        condition.resolve(ctx),
        onTrue.map(x ⇒ x.resolve(ctx)),
        onFalse.map(x ⇒ x.resolve(ctx)))
    )
  }

  override def label(label : L, body : Seq[S]) : S = {
    val (uv, dv, dl) = analyze(body)
    stmt(
      uv, dv, dl + label,
      ctx ⇒ base.label(
        ctx.label(label),
        body.map(x ⇒ x.resolve(ctx))))
  }

  override val retNone : S =
    stmt(Set.empty, Set.empty, Set.empty, ctx ⇒ base.retNone)

  override def ret(value : E) : S =
    stmt(value.usedVariables, Set.empty, Set.empty,
      ctx ⇒ base.ret(value.resolve(ctx)))

  override def chooseValue(
        discriminator : E,
        cases : Seq[(E, Seq[S])],
        onElse : Seq[S])
      : S = {
    val exprUsed = cases.foldLeft(discriminator.usedVariables)(
      (s, c) ⇒ s ++ c._1.usedVariables)
    val (uv, dv, dl) = cases.foldLeft(analyze(onElse))(
      (x, cs) ⇒  {
        val (uv2, dv2, dl2) = analyze(cs._2)
        (x._1 ++ uv2, x._2 ++ dv2, x._3 ++ dl2)
      })

    stmt(uv ++ exprUsed, dv, dl,
      ctx ⇒ base.chooseValue(
        discriminator.resolve(ctx),
        cases.map(c ⇒ (c._1.resolve(ctx), c._2.map(s ⇒ s.resolve(ctx)))),
        onElse.map(s ⇒ s.resolve(ctx)))
    )
  }

  override def raise(expr : E) : S =
    stmt(expr.usedVariables, Set.empty, Set.empty,
      ctx ⇒ base.raise(expr.resolve(ctx)))

  override def tryCatch(
        body : Seq[S],
        onException : Option[(V, Seq[S])],
        finalizer : Seq[S])
      : S = {
    val (buv, bdv, bdl) = analyze(body)
    val (fuv, fdv, fdl) = analyze(finalizer)
    onException match {
      case None ⇒
        stmt(buv ++ fuv, bdv ++ fdv, bdl ++ fdl,
          ctx ⇒ base.tryCatch(
            body.map(x ⇒ x.resolve(ctx)),
            None,
            finalizer.map(x ⇒ x.resolve(ctx)))
        )
      case Some((v, exn)) ⇒
        val (euv, edv, edl) = analyze(exn)
        stmt(buv ++ fuv ++ (euv - v), bdv ++ fdv ++ edv, bdl ++ fdl ++ edl,
          ctx ⇒ {
            val sub = ctx.sub(Set(v), Set.empty)
            base.tryCatch(
              body.map(x ⇒ x.resolve(ctx)),
              Some((sub.variable(v), exn.map(x ⇒ x.resolve(sub)))),
              finalizer.map(x ⇒ x.resolve(ctx)))
          })
    }
  }

  override def defVars(definitions : Seq[(V, Option[E])]) : S = {
    val allVars = definitions.map(_._1).toSet
    val usedVars = definitions.foldLeft(Set.empty[V])(
      (s, v) ⇒ s ++ varsOfOpt(v._2))

    stmt(usedVars, allVars, Set.empty,
      ctx ⇒ base.defVars(definitions.map(
        d ⇒ (ctx.variable(d._1), d._2.map(v ⇒ v.resolve(ctx)))))
    )
  }

  override def whileDo(cond : E, body : Seq[S]) : S = {
    val (uv, dv, dl) = analyze(body)
    stmt(uv ++ cond.usedVariables, dv, dl,
      ctx ⇒ base.whileDo(
        cond.resolve(ctx),
        body.map(x ⇒ x.resolve(ctx)))
    )
  }


  /* Local DSL. */

  /** Factory for instant expression. */
  @inline
  private def iexpr(e : BE) : E = InstantExpression(e)

  /** Factory for the deferred expression. */
  @inline
  private def dexpr(vars : Set[V], resolver : Context[V, L] ⇒ BE) =
    new UnresolvedExpression(vars, resolver)

  /** Defines an unary expression. */
  def unary(arg : E, factory : BE ⇒ BE) : E =
    arg match {
      case InstE(e) ⇒ iexpr(factory(e))
      case _ ⇒ dexpr(arg.usedVariables, ctx ⇒ factory(arg.resolve(ctx)))
    }

  /** Defines a binary expression. */
  def binary(arg1 : E, arg2 : E, factory : (BE, BE) ⇒ BE) : E =
    (arg1, arg2) match {
      case (InstE(v1), InstE(v2)) ⇒
        iexpr(factory(v1, v2))
      case _ ⇒ dexpr(
        arg1.usedVariables ++ arg2.usedVariables,
        ctx ⇒ factory(arg1.resolve(ctx), arg2.resolve(ctx)))
    }

  /** Deferred statement expression. */
  private def stmt(usedVars : Set[V], declVars : Set[V],
        declLabels : Set[L], resolver : Context[V, L] ⇒ BS)
      : S =
    new NamingStatement(usedVars, declVars, declLabels, resolver)

  /** Extracts value from optional variable. */
  private def varsOfOpt(expr : Option[E]) : Set[V] =
    expr match {
      case None ⇒ Set.empty
      case Some(x) ⇒ x.usedVariables
    }

  /** Analyzes statement sequence and returns used and declared variables
   * and declared labels.
   */
  private def analyze(body : Seq[S]) : (Set[V], Set[V], Set[L]) = {
    val allReferenced = body.foldLeft(Set.empty[V])(
        (vs, s) ⇒ vs ++ s.referencedVariables)
    val allDefined = body.foldLeft(Set.empty[V])(
        (vs, s) ⇒ vs ++ s.definedVariables)
    val allLabels = body.foldLeft(Set.empty[L])(
        (vs, s) ⇒ vs ++ s.referencedLabels)
    (allReferenced, allDefined, allLabels)
  }

  /* Utilities. */
}

/** Some static defaults. */
private object NamingSyntax {

  /** Creates a new id generator. */
  def idGen(blacklist : Set[String]) : CachingIdGenerator = {
    val base = new WildNameGenerator(ID_STARTS, ID_CHARS)
    val tamed = new TamedNameGenerator(base, blacklist ++ KEYWORDS)
    CachingIdGenerator(base.next)
  }

  /** Identifier starts. */
  val ID_STARTS = ('a' to 'z') ++ ('A' to 'Z')

  /** Identifier chars. */
  val ID_CHARS = ID_STARTS ++ ('0' to '9')

  /** Javascript keywords. */
  val KEYWORDS = Set(
    "arguments",
    "break",
    "case",
    "catch",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "finally",
    "for",
    "function",
    "if",
    "in",
    "instanceof",
    "new",
    "return",
    "switch",
    "this",
    "throw",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    "class",
    "enum",
    "export",
    "extends",
    "import",
    "super",
    "implements",
    "interface",
    "let",
    "package",
    "private",
    "protected",
    "public",
    "static",
    "yield"
  )
}
