package ru.maxkar.backend.js.out.syn

import java.math.BigDecimal
import java.math.BigInteger

/**
 * JS syntactical writer. Writes a syntactically correct code but does not
 * check for semantic validity like left-values, etc...
 * @param V type of variable definition.
 * @param L type of label definition.
 * @param E expression type.
 * @param S statement type.
 */
trait Syntax[-V, -L, E, S] {
  /** Expression for undefined property. */
  val exprUndefined : E

  /** Null expression. */
  val exprNull : E

  /** Reference to all function arguments. */
  val exprArguments : E

  /** String literal. */
  def literal(value : String) : E

  /** Numeric literal. */
  def literal(value : BigInteger) : E

  /** Floating-point literal. */
  def literal(base : BigDecimal, exp : BigInteger) : E

  /** Boolean literal. */
  def literal(value : Boolean) : E

  /** Array literal. */
  def arrayLiteral(values : Seq[E]) : E

  /** Object literal. */
  def objectLiteral(values : Seq[(String, E)]) : E

  /** Variable (or name) literal. */
  def variable(v : V) : E

  /** Function expression. */
  def functionExpr(name : Option[V], args : Seq[V], body : Seq[S]) : E

  /** Member access expression with a known member name. */
  def knownMember(base : E, accessor : String) : E

  /** Member with an unknown/dyamic accessor (member name). */
  def dynamicMember(base : E, accessor : E) : E

  /** Instance creation expression. */
  def create(specifier : E, args : Seq[E]) : E

  /** Function invocation expression. */
  def call(specifier : E, args : Seq[E]) : E

  /** Prefix increment expression. */
  def prefixInc(expr : E) : E

  /** Prefix decrement expression. */
  def prefixDec(expr : E) : E

  /** Postfix increment. */
  def postfixInc(expr : E) : E

  /** Postfix decrement. */
  def postfixDec(expr : E) : E

  /** Bitwise negation. */
  def bitNot(expr : E) : E

  /** Logical negation. */
  def boolNot(expr : E) : E

  /** Numerical negation. */
  def neg(expr : E) : E

  /** Type extraction expresson. */
  def typeOf(expr : E) : E

  /** Result dropping expression. */
  def voidOf(expr : E) : E

  /** Member destruction expression. */
  def delete(expr : E) : E

  /** Multiplication expression. */
  def mul(expr1 : E, expr2 : E) : E

  /** Division expression. */
  def div(expr1 : E, expr2 : E) : E

  /** Remainder expression. */
  def rem(expr1 : E, expr2 : E) : E

  /** Addition expression. */
  def add(expr1 : E, expr2 : E) : E

  /** Subtraction expression. */
  def sub(expr1 : E, expr2 : E) : E

  /** Left shift expression. */
  def shl(expr : E, shift : E) : E

  /** Signed shift right expression. */
  def sshr(expr : E, shift : E) : E

  /** Unsigned shift right expression. */
  def ushr(expr : E, shift : E) : E

  /** "Lesser" comparison. */
  def less(expr1 : E, expr2 : E) : E

  /** "Not greater" comparison. */
  def lessEq(expr1 : E, expr2 : E) : E

  /** "Greater" comparison. */
  def greater(expr1 : E, expr2 : E) : E

  /** "Not less" comparison. */
  def greaterEq(expr1 : E, expr2 : E) : E

  /** "Is in" check. */
  def isIn(expr : E, collection : E) : E

  /** Check instance ascentor. */
  def isInstanceOf(expr : E, ascentor : E) : E

  /** Equality check. */
  def equals(expr1 : E, expr2 : E) : E

  /** Nonequality check. */
  def notEquals(expr1 : E, expr2 : E) : E

  /** Strict equality check (identity comparison). */
  def strictEquals(expr1 : E, expr2 : E) : E

  /** Strict unequality check (identity comparison). */
  def strictNotEquals(expr1 : E, expr2 : E) : E

  /** Bitwise and. */
  def bitAnd(expr1 : E, expr2 : E) : E

  /** Bitwise exclusive or. */
  def bitXor(expr1 : E, expr2 : E) : E

  /** Bitwise or. */
  def bitOr(expr1 : E, expr2 : E) : E

  /** Logical and. */
  def boolAnd(expr1 : E, expr2 : E) : E

  /** Logical or. */
  def boolOr(expr1 : E, expr2 : E) : E

  /** Conditional (ternary) expression. */
  def condExpr(condition : E, whenTrue : E, whenFalse : E) : E

  /** Assinment expression. */
  def assign(host : E, value : E) : E

  /** Inplace addition. */
  def inplaceAdd(host : E, value : E) : E

  /** Inplace subtraction. */
  def inplaceSub(host : E, value : E) : E

  /** Inplace multiplication. */
  def inplaceMul(host : E, value : E) : E

  /** Inplace division. */
  def inplaceDiv(host : E, value : E) : E

  /** Inplace remainder. */
  def inplaceRem(host : E, value : E) : E

  /** Inplace left shift. */
  def inplaceShl(host : E, shift : E) : E

  /** Inplace signed right shift. */
  def inplaceSshr(host : E, shift : E) : E

  /** Inplace unsigned right shift. */
  def inplaceUshr(host : E, shift : E) : E

  /** Inplace bitwise and. */
  def inplaceBitAnd(host : E, value : E) : E

  /** Inplace bitwise exclusive or. */
  def inplaceBitXor(host : E, value : E) : E

  /** Inplace bitwise or. */
  def inplaceBitOr(host : E, value : E) : E

  /** Sequence expression. */
  def seqExpr(first : E, tail : Seq[E]) : E

  /** Creates statement from expression. */
  def exprStatement(expr : E) : S

  /** Break outer control. */
  val breakOuter : S

  /** Break a labeled statement. */
  def breakLabeled(label : L) : S

  /** Continues outer loop. */
  val continueOuter : S

  /** Continues a labeled loop. */
  def continueLabeled(label : L) : S

  /** Performs a loop until expression became false. */
  def doWhile(body : Seq[S], condition : E) : S

  /** Performs a classical three-clause for loop with variable declarations. */
  def forVars(
      variables : Seq[(V, Option[E])],
      condition : Option[E],
      finalExpression : Option[E],
      body : Seq[S])
    : S

  /** Classical three-clause for loop without variable declarations. */
  def forExisting(
      init : Option[E],
      condition : Option[E],
      finalExpression : Option[E],
      body : Seq[S])
    : S

  /** Iterates on keys of the collection using a new variable. */
  def forVariableIn(variable : V, collection : E, body : Seq[S]) : S

  /** Iterates over collection using an existing variable as iterator. */
  def forExistingIn(iterator : E, collection : E, body : Seq[S]) : S

  /** Defines a function as a statement. */
  def functionStmt(name : Option[V], args : Seq[V], body : Seq[S]) : S

  /** Conditional statement. */
  def ifStatement(condition : E, onTrue : Seq[S], onFalse : Seq[S]) : S

  /** Labels a statement or a group of statements. */
  def label(name : L, body : Seq[S]) : S

  /** "Return nothing" statement. */
  val retNone : S

  /** "Return value" statement. */
  def ret(value : E) : S

  /** Choice (value matching) expression. */
  def chooseValue(
      discriminator : E,
      cases : Seq[(E, Seq[S])],
      onElse : Seq[S])
    : S

  /** Raises an exception. */
  def raise(v : E) : S

  /** Try-catch statement. */
  def tryCatch(
      body : Seq[S],
      onException : Option[(V, Seq[S])],
      finalizer : Seq[S])
    : S

  /** Variable declatation statement. */
  def defVars(definitions : Seq[(V, Option[E])]) : S

  /** Loop with a prefix condition check. */
  def whileDo(condition : E, body : Seq[S]) : S
}
