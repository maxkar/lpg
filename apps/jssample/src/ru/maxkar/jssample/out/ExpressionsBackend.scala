package ru.maxkar.jssample.out

import java.math.BigInteger
import java.math.BigDecimal


/** Expression backend. */
trait ExpressionsBackend[E, S] {
  /** Calls an expression. */
  def call(host : E, arguments : Seq[E]) : E

  /** Creates an integer literas. */
  def literal(value : BigInteger) : E

  /** Creates a floating-point literal. */
  def literal(value : BigDecimal, exponent : BigInteger) : E

  /** Creates a string literal. */
  def literal(value : String) : E

  /** Defines a variable reference. */
  def variable(id : AnyRef) : E

  /** Defines an anonymous function. */
  def anonfun(args : Seq[AnyRef], body : Seq[S]) : E

  /** Truth expression. */
  val exprTrue : E
  /** Falsiness expression. */
  val exprFalse : E
  /** Null expression. */
  val exprNull : E
  /** Undefined expression. */
  val exprUndefined : E
  /** Arguments reference. */
  val exprArguments : E
  /** Array creator expression host. */
  val arrayCreator : E
  /** Map creator expression. */
  val mapCreator : E
  /** New instance creation expression. */
  val instanceCreator : E
  /** Two-way choice expression host. */
  val choiceExpression : E
  /** Prefix increment host. */
  val prefixInc : E
  /** Prefix decrement host. */
  val prefixDec : E
  /** Postfix increment host. */
  val postfixInc : E
  /** Postfix decrement host. */
  val postfixDec : E
  /** Member deletion host. */
  val delMember : E
  /** Logical negation host. */
  val boolNot : E
  /** Bitwise negation host. */
  val bitNot : E
  /** Integral negation host. */
  val neg : E
  /** Type extraction host. */
  val typeOf : E
  /** Member extraction host. */
  val member : E
  /** Multiplication function. */
  val mul : E
  /** Division function. */
  val div : E
  /** Remainder function. */
  val rem : E
  /** Addition function. */
  val add : E
  /** Subtraction function. */
  val sub : E
  /** Left shift function. */
  val shl : E
  /** Signed right shift function. */
  val sshr : E
  /** Unsigned right shift function. */
  val ushr : E
  /** "Lesser" comparison function. */
  val less : E
  /** "Less or equals" comparison function. */
  val lessEq : E
  /** "Greater" comparison function. */
  val greater : E
  /** "Greater or equals comparios function. */
  val greaterEq : E
  /** Key containment check. */
  val isIn : E
  /** Instance check. */
  val testInstanceOf : E
  /** Equality check. */
  val equalsTo : E
  /** Inequality check. */
  val notEquals : E
  /** Reference equality check. */
  val strictEquals : E
  /** Reference unequality check. */
  val strictNotEquals : E
  /** Bitwise and. */
  val bitAnd : E
  /** Bitwise xor. */
  val bitXor : E
  /** Bitwise or. */
  val bitOr : E
  /** Boolean and. */
  val boolAnd : E
  /** Boolean OR. */
  val boolOr : E
  /** Inplace addition host. */
  val inplaceAdd : E
  /** Inplace subtraction host. */
  val inplaceSub : E
  /** Inplace multiplication host. */
  val inplaceMul : E
  /** Inplace division host. */
  val inplaceDiv : E
  /** Inplace rpmanider host. */
  val inplaceRem : E
  /** Inplace left shift host. */
  val inplaceShl : E
  /** Inplace signed right shift host. */
  val inplaceSshr : E
  /** Inplace unsigned right shift host. */
  val inplaceUshr : E
  /** Inplace bitwise and. */
  val inplaceBitAnd : E
  /** Inplace bitwise or. */
  val inplaceBitOr : E
  /** Inplace bitwise xor. */
  val inplaceBitXor : E
}
