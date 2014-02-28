package ru.maxkar.backend.js.model

import java.math.BigInteger
import java.math.BigDecimal

import ru.maxkar.backend.js.out.CompactContext
import ru.maxkar.backend.js.out.writer.Writer
import ru.maxkar.backend.js.out.writer.ContextWriter

import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer

import ru.maxkar.backend.js.out.syn._
import ru.maxkar.backend.js.out.syn.naming._


/** Model element factories.
 * Lacks support of regular expressions at this point.
 */
final object Model {

  /** Fragment type. */
  type F = java.io.Writer ⇒ Unit

  /** Expression type. */
  type E = NamingExpression[AnyRef, AnyRef, CompactExpression[F]]

  /** Statement type. */
  type S = NamingStatement[AnyRef, AnyRef, CompactStatement[F]]

  /** Bottom model. */
  private val peer1 = new CompactSyntax(WriteClass)

  /** Second-level syntax implementation. */
  private val peer2 = new
    NamingSyntax[AnyRef, AnyRef, CompactExpression[F], CompactStatement[F]](peer1)

  /** Real underlying model. */
  private val model : Syntax[AnyRef, AnyRef, E, S] = peer2

  /** Failure expression. */
  val failure : LeftValue = new LeftValue(null)

  /** Boolean "true" expression. */
  val exprTrue : Expression = new Expression(model.literal(true))


  /** Boolean "false" expression. */
  val exprFalse : Expression = new Expression(model.literal(false))


  /** Null expression. */
  val exprNull : Expression = new Expression(model.exprNull)


  /** Undefined expression. */
  val undefined : Expression = new Expression(model.exprUndefined)


  /** Function arguments expression. */
  val arguments : Expression = new Expression(model.exprArguments)


  /** Creates a function definition. */
  def defun(id : AnyRef, args : Seq[AnyRef], locals : Seq[AnyRef],
      localFuncs : Seq[(AnyRef, FunctionBody)],
      labels : Seq[AnyRef],
      body : Seq[Statement]) : (AnyRef, FunctionBody) =
    (id, mkFunctionBody(args, locals, localFuncs, labels, body))


  /** Creates a string expression. */
  def literal(expr : String) : Expression =
    new Expression(model.literal(expr),
      if (validSimpleId(expr)) Some(expr) else None)


  /** Creates a number literal expression. */
  def literal(value : Int) : Expression =
    literal(BigInteger.valueOf(value))


  /** Creates a numeric literal expression. */
  def literal(value : Long) : Expression =
    literal(BigInteger.valueOf(value))


  /** Creates a numeric literal experssion. */
  def literal(value : BigInteger) : Expression =
    new Expression(model.literal(value))


  /** Creates a number literal expression. */
  def literal(head : BigDecimal, tail : BigInteger) : Expression =
    new Expression(model.literal(head, tail))


  /** Creates a boolean literal. */
  def literal(value : Boolean) : Expression =
    if (value) exprTrue else exprFalse


  /** Creates an array expression. */
  def arrayliteral(elts : Expression*) : Expression =
    new Expression(model.arrayLiteral(exprs2base(elts)))


  /** Creates an object literal. */
  def objectliteral(elts : (String, Expression)*) : Expression =
    new Expression(model.objectLiteral(
      elts map (
        x ⇒ (x._1, expr2base(x._2))))
    )


  /** Creates a reference to a variable in outer scope. */
  def variable(ref : AnyRef) : LeftValue =
    new LeftValue(model.variable(ref))


  /** Creates an anonymous local function. */
  def anonfun(args : Seq[AnyRef], locals : Seq[AnyRef],
      localFuncs : Seq[(AnyRef, FunctionBody)],
      labels : Seq[AnyRef],
      body : Seq[Statement]) : Expression = {

    val locDef =
      if (locals.isEmpty)
        Seq.empty[S]
      else
        Seq(model.defVars(locals.map(x ⇒  (x, None))))

    val locFuncs =
      localFuncs.map(
        x ⇒ model.functionStmt(Some(x._1), x._2.args, x._2.statements))

    new Expression(model.functionExpr(None, args,
      locDef ++ locFuncs ++ stmts2base(body)))
  }


  /** Creates a named (self-referentiable) ocal function. */
  def namedfun(id : AnyRef, args : Seq[AnyRef], locals : Seq[AnyRef],
      localFuncs : Seq[(AnyRef, FunctionBody)],
      labels : Seq[AnyRef],
      body : Seq[Statement]) : Expression = {
    val locDef =
      if (locals.isEmpty)
        Seq.empty[S]
      else
        Seq(model.defVars(locals.map(x ⇒  (x, None))))

    val locFuncs =
      localFuncs.map(
        x ⇒ model.functionStmt(Some(x._1), x._2.args, x._2.statements))

    new Expression(model.functionExpr(Some(id), args,
      locDef ++ locFuncs ++ stmts2base(body)))
  }


  /** Member access expression. */
  def member(base : Expression, item : Expression) : LeftValue =
    item.simpleMemberAccessor match {
      case None ⇒
        new LeftValue(model.dynamicMember(base.base, item.base))
      case Some(x) ⇒
        new LeftValue(model.knownMember(base.base, x))
    }


  /** Creates a new instance creation expression. */
  def create(base : NonprimitiveExpression, args : Expression*) : NonprimitiveExpression =
    new NonprimitiveExpression(model.create(base.base, exprs2base(args)))


  /** Writes a function call. */
  def call(base : NonprimitiveExpression, args : Expression*) : NonprimitiveExpression =
    new NonprimitiveExpression(model.call(base.base, exprs2base(args)))

  /** Prefix increment expression. */
  def prefixInc(base : LeftValue) : Expression =
    new Expression(model.prefixInc(base.base))


  /** Prefix decrement expression. */
  def prefixDec(base : LeftValue) : Expression =
    new Expression(model.prefixDec(base.base))


  /** Postfix increment expression. */
  def postfixInc(base : LeftValue) : Expression =
    new Expression(model.postfixInc(base.base))


  /** Postfix decrement expression. */
  def postfixDec(base : LeftValue) : Expression =
    new Expression(model.postfixDec(base.base))


  /** Logical not. */
  def boolNot(base : Expression) : Expression =
    new Expression(model.boolNot(base.base))


  /** Bitwise not. */
  def bitNot(base : Expression) : Expression =
    new Expression(model.bitNot(base.base))


  /** Negation expression. */
  def neg(base : Expression) : Expression =
    new Expression(model.neg(base.base))


  /** "Type of" expression. */
  def typeof(base : Expression) : Expression =
    new Expression(model.typeOf(base.base))


  /** "Void" expression. */
  def voidof(base : Expression) : Expression =
    new Expression(model.voidOf(base.base))


  /** "Delete" expression. */
  def delete(base : NonprimitiveExpression) : Expression =
    new Expression(model.delete(base.base))


  /** Multiplication expression. */
  def mul(left : Expression, right : Expression) : Expression =
    new Expression(model.mul(left.base, right.base))


  /** Division expression. */
  def div(left : Expression, right : Expression) : Expression =
    new Expression(model.div(left.base, right.base))


  /** Remainder expression. */
  def rem(left : Expression, right : Expression) : Expression =
    new Expression(model.rem(left.base, right.base))


  /** Addition expression. */
  def add(left : Expression, right : Expression) : Expression =
    new Expression(model.add(left.base, right.base))


  /** Subtraction expression. */
  def sub(left : Expression, right : Expression) : Expression =
    new Expression(model.sub(left.base, right.base))


  /** Shift-left expression. */
  def shl(left : Expression, right : Expression) : Expression =
    new Expression(model.shl(left.base, right.base))


  /** Signed shift right expression. */
  def sshr(left : Expression, right : Expression) : Expression =
    new Expression(model.sshr(left.base, right.base))


  /** Unsighed shift right expression. */
  def ushr(left : Expression, right : Expression) : Expression =
    new Expression(model.ushr(left.base, right.base))


  /** Less comparison expression. */
  def less(left : Expression, right : Expression) : Expression =
    new Expression(model.less(left.base, right.base))


  /** Less or equals comparison expression. */
  def lessEq(left : Expression, right : Expression) : Expression =
    new Expression(model.lessEq(left.base, right.base))


  /** Greater comparison expression. */
  def greater(left : Expression, right : Expression) : Expression =
    new Expression(model.greater(left.base, right.base))


  /** Greator or equals expression. */
  def greaterEq(left : Expression, right : Expression) : Expression =
    new Expression(model.greaterEq(left.base, right.base))


  /** "In" check expression. */
  def isIn(left : Expression, right : Expression) : Expression =
    new Expression(model.isIn(left.base, right.base))


  /** Conditional cast expression. */
  def testInstanceOf(left : Expression, right : Expression) : Expression =
    new Expression(model.isInstanceOf(left.base, right.base))


  /** Equals expression. */
  def equalsTo(left : Expression, right : Expression) : Expression =
    new Expression(model.equals(left.base, right.base))


  /** Not equals expression. */
  def notEquals(left : Expression, right : Expression) : Expression =
    new Expression(model.notEquals(left.base, right.base))


  /** Strict equality comparison. */
  def strictEquals(left : Expression, right : Expression) : Expression =
    new Expression(model.strictEquals(left.base, right.base))


  /** String non-equals expression. */
  def strictNotEquals(left : Expression, right : Expression) : Expression =
    new Expression(model.strictNotEquals(left.base, right.base))


  /** Bitwise and expression. */
  def bitAnd(left : Expression, right : Expression) : Expression =
    new Expression(model.bitAnd(left.base, right.base))


  /** Bitwise exclusive or expression. */
  def bitXor(left : Expression, right : Expression) : Expression =
    new Expression(model.bitXor(left.base, right.base))


  /** Bitwise or expression. */
  def bitOr(left : Expression, right : Expression) : Expression =
    new Expression(model.bitOr(left.base, right.base))


  /** Boolean and expression. */
  def boolAnd(left : Expression, right : Expression) : Expression =
    new Expression(model.boolAnd(left.base, right.base))


  /** Boolean or expression. */
  def boolOr(left : Expression, right : Expression) : Expression =
    new Expression(model.boolOr(left.base, right.base))


  /** Conditional expression. */
  def cond(cond : Expression,
      onTrue : Expression, onFalse: Expression) : Expression =
    new Expression(model.condExpr(cond.base, onTrue.base, onFalse.base))


  /** Assignment expression. */
  def assign(host : LeftValue, value : Expression) : Expression =
    new Expression(model.assign(host.base, value.base))


  /** Inplace addition expression. */
  def inplaceAdd(host : LeftValue, value : Expression) : Expression =
    new Expression(model.inplaceAdd(host.base, value.base))


  /** Inplace subtraction expression. */
  def inplaceSub(host : LeftValue, value : Expression) : Expression =
    new Expression(model.inplaceSub(host.base, value.base))


  /** Inplace multiplication expression. */
  def inplaceMul(host : LeftValue, value : Expression) : Expression =
    new Expression(model.inplaceMul(host.base, value.base))


  /** Inplace division expression. */
  def inplaceDiv(host : LeftValue, value : Expression) : Expression =
    new Expression(model.inplaceDiv(host.base, value.base))


  /** Inplace remainder expression. */
  def inplaceRem(host : LeftValue, value : Expression) : Expression =
    new Expression(model.inplaceRem(host.base, value.base))


  /** Inplace shilt left expression. */
  def inplaceShl(host : LeftValue, value : Expression) : Expression =
    new Expression(model.inplaceShl(host.base, value.base))


  /** Inplace signed shift right expression. */
  def inplaceSshr(host : LeftValue, value : Expression) : Expression =
    new Expression(model.inplaceSshr(host.base, value.base))


  /** Inplace unsigned shift right expression. */
  def inplaceUshr(host : LeftValue, value : Expression) : Expression =
    new Expression(model.inplaceUshr(host.base, value.base))


  /** Inplace bitwise and expression. */
  def inplaceBitAnd(host : LeftValue, value : Expression) : Expression =
    new Expression(model.inplaceBitAnd(host.base, value.base))


  /** Inplace bitwise or expression. */
  def inplaceBitOr(host : LeftValue, value : Expression) : Expression =
    new Expression(model.inplaceBitOr(host.base, value.base))


  /** Inplace bitwise exclusive or expression. */
  def inplaceBitXor(host : LeftValue, value : Expression) : Expression =
    new Expression(model.inplaceBitXor(host.base, value.base))


  /** Sequence (comma) expression. */
  def seqExpr(first : Expression, second : Expression) : Expression =
    new Expression(model.seqExpr(first.base, Seq(second.base)))

  /** Converts expression into statement. */
  implicit def expr2statement(expr : Expression) : Statement =
    new Statement(model.exprStatement(expr.base))


  /** Breaks from an outer statement. */
  def breakOuter() : Statement = new Statement(model.breakOuter)


  /** Breaks a labeled statement. */
  def breakL(label : AnyRef) = new Statement(model.breakLabeled(label))


  /** Continues an outer statement. */
  def continueOuter() : Statement = new Statement(model.continueOuter)


  /** Continues a labeled statement. */
  def continueL(label : AnyRef) = new Statement(model.continueLabeled(label))


  /** Preforms statement with a postfix condition check. */
  def doWhile(items : Seq[Statement], cond : Expression) : Statement =
    new Statement(model.doWhile(stmts2base(items), cond.base))


  /** Creates a for statement. */
  def whileWithIterupdate(cond : Expression, update : Expression,
      body : Seq[Statement]) : Statement =
    new Statement(model.forExisting(None, Some(cond.base), Some(update.base),
      stmts2base(body)))


  /** Performs iteration over container keys. */
  def forIn(iter : LeftValue, collection : Expression,
      body : Seq[Statement]) : Statement =
    new Statement(model.forExistingIn(iter.base, collection.base,
      stmts2base(body)))


  /** Performs statements when condition is true. */
  def when(cond : Expression, body : Seq[Statement]) : Statement =
    doCond(cond, body, Seq.empty)


  /** Chooses one of two statements. */
  def doCond(condition : Expression, onTrue : Seq[Statement],
      onFalse : Seq[Statement]): Statement =
    new Statement(model.ifStatement(condition.base,
      stmts2base(onTrue), stmts2base(onFalse)))


  /** Labels a statement. */
  def label(lbl : AnyRef, body : Statement) : Statement =
    new Statement(model.label(lbl, Seq(body.base)))


  /** Return statement. */
  def returns(value : Expression) : Statement =
    new Statement(model.ret(value.base))


  /** Return-nothing statement. */
  def returnNothing() : Statement = new Statement(model.retNone)


  /** Switch statement. */
  def switchof(cond : Expression,
      rmap : Seq[(Seq[Expression], Seq[Statement])],
      onElse : Option[Seq[Statement]]) : Statement = {
    val conds = new ArrayBuffer[(E, Seq[S])]

    for ((bases, code) ← rmap) {
      for (base ← bases.dropRight(1))
        conds += ((base.base, Seq.empty))
      conds += ((bases.last.base, stmts2base(code) :+ model.breakOuter))
    }

    val edata = onElse match {
      case None ⇒ Seq.empty[S]
      case Some(x) ⇒ stmts2base(x)
    }

    new Statement(model.chooseValue(cond.base, conds, edata))
  }


  /** Throw statement. */
  def throws(value : Expression) : Statement =
    new Statement(model.raise(value.base))


  /** Try/catch statement. */
  def tryCatch( body : Seq[Statement],
      exnId : AnyRef,
      exnHandler : Seq[Statement]) : Statement =
    new Statement(model.tryCatch(
      stmts2base(body),
      Some((exnId, stmts2base(exnHandler))),
      Seq.empty
    ))


  /** Tries with finalizer. */
  def withFin(body : Seq[Statement], fin : Seq[Statement]) : Statement =
    new Statement(model.tryCatch(
      stmts2base(body),
      None,
      stmts2base(fin)))


  /** While statement. */
  def whiles(cond : Expression, body : Seq[Statement]) : Statement =
    new Statement(model.whileDo(cond.base, stmts2base(body)))


  /** Creates a javascript file. */
  def file(
        globals : Seq[(AnyRef, String)] = Seq.empty,
        vars : Seq[AnyRef] = Seq.empty,
        funcs :Seq[(AnyRef, FunctionBody)] = Seq.empty,
        inits : Seq[Statement] = Seq.empty) :
      JSFile = {

    val uniqueNames = globals.map(_._2).toSet
    if (uniqueNames.size != globals.size)
      throw new IllegalArgumentException("Non-unique global name present")

    val vstmts =
      if (vars.isEmpty)
        Seq.empty[S]
      else
        Seq(model.defVars(vars.map(x ⇒ ((x, None)))))

    val fnstmts =
      funcs.map(f ⇒
        model.functionStmt(Some(f._1), f._2.args, f._2.statements))

    val r2 = peer2.compile(globals.toMap,
      vstmts ++ fnstmts ++ stmts2base(inits))
    new JSFile(peer1.compile(r2))
  }


  /**
   * Outputs js file into a given writer.
   * @param file file to write.
   * @param w target stream.
   */
  def writeFileToWriter(file : JSFile, w : java.io.Writer) : Unit =
    file.writer(w)


  /** Extracts base from expression. */
  private def expr2base(e : Expression) : E =
    e.base

  /** Extracts base from expressions. */
  private def exprs2base(exprs : Seq[Expression]) : Seq[E] =
    exprs map expr2base

  /** Extracts base from statement. */
  private def stmt2base(stmt : Statement) : S =
    stmt.base

  /** Extracts bases from statements. */
  private def stmts2base(stmts : Seq[Statement]) : Seq[S] =
    stmts map stmt2base

  /** Checks, if value is valid identifier. */
  private def validSimpleId(value :String) : Boolean = {
    if (value.isEmpty)
      return false
    if (Character.isDigit(value.charAt(0)))
      return false

    var ptr = 0
    while (ptr < value.length) {
      if (!isValidIdentifierChar(value.charAt(ptr)))
        return false
      ptr+=1
    }

    true
  }


  /** Checks, if character is valid identifier char. */
  private def isValidIdentifierChar(c : Char) : Boolean = {
    Character.getType(c) match {
      case
        Character.UPPERCASE_LETTER |
        Character.LOWERCASE_LETTER |
        Character.TITLECASE_LETTER |
        Character.MODIFIER_LETTER |
        Character.OTHER_LETTER |
        Character.LETTER_NUMBER ⇒ true
      case _ ⇒ false
    }
  }


  def mkFunctionBody(
      args : Seq[AnyRef],
      vars : Seq[AnyRef],
      funcs : Seq[(AnyRef, FunctionBody)],
      labels : Seq[AnyRef],
      stmt : Seq[Statement]) : FunctionBody = {

    val vstmts =
      if (vars.isEmpty)
        Seq.empty[S]
      else
        Seq(model.defVars(vars.map(x ⇒ ((x, None)))))

    val fnstmts =
      funcs.map(f ⇒
        model.functionStmt(Some(f._1), f._2.args, f._2.statements))

    new FunctionBody(args, vstmts ++ fnstmts ++ stmts2base(stmt))
  }
}
