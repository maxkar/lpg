package ru.maxkar.jssample.out.js

import ru.maxkar.backend.js.out.syn.Syntax

import java.math.BigInteger
import java.math.BigDecimal

import scala.collection.mutable.ArrayBuffer

/**
 * Javascript backend implementation.
 * @param base underlying syntax.
 * @param BE base expression type.
 * @param BS base statement type.
 */
final class Backend[BE, BS](base : Syntax[AnyRef, AnyRef, BE, BS])
    extends ru.maxkar.jssample.out.ExpressionsBackend[Expression[BE, BS], Statement[BE, BS]] {
  /** Expression type. */
  type E = Expression[BE, BS]
  /** Statement type. */
  type S = Statement[BE, BS]


  /** Expression invocation. */
  override def call(host : E, arguments : Seq[E]) : E =
    host.invokeHandler(host, arguments)


  override def literal(value : BigInteger) : E =
    mkLiteral(base.literal(value))


  override def literal(value : BigDecimal, exponent : BigInteger) : E =
    mkLiteral(base.literal(value, exponent))


  override def literal(value : String) : E =
    mkLiteral(base.literal(value))


  override val exprTrue : E = mkLiteral(base.literal(true))


  override val exprFalse : E = mkLiteral(base.literal(false))


  override val exprNull : E = mkLiteral(base.exprNull)


  override val exprUndefined : E = mkLiteral(base.exprUndefined)


  override val exprArguments : E = mkLiteral(base.exprArguments)


  override def variable(id : AnyRef) : E =
    new Expression(base.variable(id), None, standardInvoke, Set.empty)


  override def anonfun(args : Seq[AnyRef], body : Seq[S]) : E =
    new Expression(
      base.functionExpr(None, args, body.map(_.base)),
      None,
      standardInvoke,
      body.foldLeft(Set.empty[FirstClassWrapper[BS]])(
        (s, a) ⇒ s ++ a.firstClassOperators))


  override val arrayCreator : E = {
    val lenId = new Object
    val len = base.variable(lenId)
    val iId = new Object
    val i = base.variable(iId)
    val resId = new Object
    val res = base.variable(resId)

    mkSpecialOperator(arrayInvoke, Seq.empty, Seq(
      base.defVars(Seq(
        (lenId, Some(base.knownMember(base.exprArguments, "length"))),
        (resId, Some(base.arrayLiteral(Seq.empty))))),
      base.forVars(
        Seq((iId, Some(base.literal(BigInteger.ZERO)))),
        Some(base.less(i, len)),
        Some(base.postfixInc(i)),
        Seq(base.exprStatement(
          base.call(
            base.knownMember(res, "push"),
            Seq(
              base.dynamicMember(base.exprArguments, i)
            ))
      ))),
      base.ret(res)
    ))
  }


  override val mapCreator : E = {
    val lenId = new Object
    val len = base.variable(lenId)
    val iId = new Object
    val i = base.variable(iId)
    val resId = new Object
    val res = base.variable(resId)

    mkSpecialOperator(objectInvoke, Seq.empty, Seq(
      base.defVars(Seq(
        (lenId, Some(base.knownMember(base.exprArguments, "length"))),
        (resId, Some(base.objectLiteral(Seq.empty))))),
      base.forVars(
        Seq((iId, Some(base.literal(BigInteger.ZERO)))),
        Some(base.less(i, len)),
        None,
        Seq(base.exprStatement(
          base.assign(
            base.dynamicMember(res,
              base.dynamicMember(base.exprArguments,
                base.postfixInc(i))),
            base.dynamicMember(base.exprArguments,
              base.postfixInc(i)))
      ))),
      base.ret(res)
    ))
  }


  override val instanceCreator : E =
    new Expression(base.exprUndefined, None, newInvoke, Set.empty)


  override val choiceExpression : E = {
    val a1id = new Object
    val a2id = new Object
    val a3id = new Object

    mkSpecialOperator(choiceInvoke, Seq(a1id, a2id, a3id), Seq(
      base.exprStatement(
        base.condExpr(
          base.variable(a1id),
          base.variable(a2id),
          base.variable(a3id)))))
  }


  override val prefixInc : E = unaryAssign(base.prefixInc)
  override val prefixDec : E = unaryAssign(base.prefixDec)
  override val postfixInc : E = unaryAssign(base.postfixInc)
  override val postfixDec : E = unaryAssign(base.postfixDec)
  override val delMember : E = unaryAssign(base.delete)

  override val boolNot : E = unary(base.boolNot)
  override val bitNot : E = unary(base.bitNot)
  override val neg : E = unary(base.neg)
  override val typeOf : E = unary(base.typeOf)


  override val member : E =
    mkSpecialOperator(
      binaryMemberInvoke,
      Seq.empty,
      mkFoldArgs(base.dynamicMember))

  override val mul : E = binary(base.mul)
  override val div : E = binary(base.div)
  override val rem : E = binary(base.rem)
  override val add : E = binary(base.add)
  override val sub : E = binary(base.sub)
  override val shl : E = binary(base.shl)
  override val sshr : E = binary(base.sshr)
  override val ushr : E = binary(base.ushr)
  override val less : E = binary(base.less)
  override val lessEq : E = binary(base.lessEq)
  override val greater : E = binary(base.greater)
  override val greaterEq : E = binary(base.greaterEq)
  override val isIn : E = binary(base.isIn)
  override val testInstanceOf : E = binary(base.isInstanceOf)
  override val equalsTo : E = binary(base.equals)
  override val notEquals : E = binary(base.notEquals)
  override val strictEquals : E = binary(base.strictEquals)
  override val strictNotEquals : E = binary(base.strictNotEquals)
  override val bitAnd : E = binary(base.bitAnd)
  override val bitXor : E = binary(base.bitXor)
  override val bitOr : E = binary(base.bitOr)
  override val boolAnd : E = binary(base.boolAnd)
  override val boolOr : E = binary(base.boolOr)

  override val inplaceAdd : E = binaryAssign(base.inplaceAdd)
  override val inplaceSub : E = binaryAssign(base.inplaceSub)
  override val inplaceMul : E = binaryAssign(base.inplaceMul)
  override val inplaceDiv : E = binaryAssign(base.inplaceDiv)
  override val inplaceRem : E = binaryAssign(base.inplaceRem)
  override val inplaceShl : E = binaryAssign(base.inplaceShl)
  override val inplaceSshr : E = binaryAssign(base.inplaceSshr)
  override val inplaceUshr : E = binaryAssign(base.inplaceUshr)
  override val inplaceBitAnd : E = binaryAssign(base.inplaceBitAnd)
  override val inplaceBitOr : E = binaryAssign(base.inplaceBitOr)
  override val inplaceBitXor : E = binaryAssign(base.inplaceBitXor)


  /** Creates new literal. */
  private def mkLiteral(value : BE) : E =
    new Expression(value, None, uninvokable, Set.empty)


  /** Invoker for the uninvokable expressions. */
  private def uninvokable(host : E, args : Seq[E]) : E =
    new Expression(base.exprUndefined, None, uninvokable, Set.empty)


  /** Standard invokation processor. */
  private def standardInvoke(host : E, args : Seq[E]) : E =
    new Expression(
      base.call(host.base, args.map(_.base)),
      None,
      standardInvoke,
      aggSpecE(args))


  /** Array creation invoker. */
  private def arrayInvoke(host : E, args : Seq[E]) : E =
    new Expression(
      base.arrayLiteral(args.map(_.base)),
      None,
      uninvokable,
      aggSpecE(args))


  /** Object creation invoker. */
  private def objectInvoke(host : E, args : Seq[E]) : E = {
    val realArgs =
      if (args.length / 2 == 0)
        args
      else
        args :+ exprUndefined

    val itr = realArgs.iterator
    val rb = new ArrayBuffer[(String, BE)](args.length / 2)
    val specials = aggSpecE(args)

    while (itr.hasNext) {
      val e1 = itr.next
      e1.constValue match {
        case None ⇒
          return new Expression(
            base.call(host.base, args.map(_.base)),
            None, uninvokable, specials ++ host.firstClassOperators)
        case Some(x) ⇒
          rb += ((x, itr.next.base))
      }
    }

    new Expression(
      base.objectLiteral(rb),
      None, uninvokable, specials)
  }


  /** Instance creation invokation handler. */
  private def newInvoke(host : E, args : Seq[E]) : E = {
    if (args.length == 0)
      exprUndefined
    else
      new Expression(
        base.create(args.head.base, args.tail.map(_.base)),
        None, standardInvoke, aggSpecE(args))
  }


  /** Simple choice invokation. */
  private def choiceInvoke(host : E, args : Seq[E]) : E = {
    args match {
      case Seq() ⇒ exprUndefined
      case Seq(x) ⇒
        new Expression(
          base.condExpr(x.base, base.exprUndefined, base.exprUndefined),
          None, standardInvoke, x.firstClassOperators)
      case Seq(x, y) ⇒
        new Expression(
          base.condExpr(x.base, y.base, base.exprUndefined),
          None, standardInvoke,
          x.firstClassOperators ++ y.firstClassOperators)
      case Seq(x, y, z) ⇒
        new Expression(
          base.condExpr(x.base, y.base, z.base),
          None, standardInvoke,
          aggSpecE(args))
      case _ ⇒
        new Expression(
          base.call(host.base, args.map(_.base)),
          None, standardInvoke,
          aggSpecE(args) ++ host.firstClassOperators)
    }
  }

  /** Unary invokation. */
  private def unaryInvoke(baseUnary : BE ⇒ BE, host : E, args : Seq[E]) =
    args match {
      case Seq() ⇒ exprUndefined
      case Seq(x) ⇒
        new Expression(
          baseUnary(x.base),
          None, standardInvoke,
          x.firstClassOperators)
      case _ ⇒
        new Expression(
          base.seqExpr(
            baseUnary(args.head.base),
            args.tail.map(x ⇒ baseUnary(x.base))),
          None, standardInvoke,
          aggSpecE(args))
    }

  /** Unary operator factory. */
  private def unary(baseUnary : BE ⇒ BE) : E =
    mkSpecialOperator(
      (x, y) ⇒ unaryInvoke(baseUnary, x, y),
      Seq.empty,
      Seq(base.ret(
        baseUnary(
          base.dynamicMember(
            base.exprArguments,
            base.sub(
              base.knownMember(base.exprArguments, "length"),
              base.literal(BigInteger.valueOf(-1))))))))


  /** Unary assignment factory. */
  private def unaryAssign(baseUnary : BE ⇒ BE) : E =
    new Expression(
      base.exprUndefined,
      None,
      (x, y) ⇒ unaryInvoke(baseUnary, x, y),
      Set.empty)


  /** Binary expression invocation. */
  private def binaryStdInvoke(baseBinary : (BE, BE) ⇒ BE, host : E, args : Seq[E]) : E = {
    args match {
      case Seq() ⇒ exprUndefined
      case Seq(x) ⇒ x
      case _ ⇒
        new Expression(
          args.tail.foldLeft(args.head.base)(
            (x, y) ⇒ baseBinary(x, y.base)),
          None,
          standardInvoke,
          aggSpecE(args))
    }
  }


  /** Binary assignment invocation. */
  private def binaryAssignInvoke(baseBinary : (BE, BE) ⇒ BE, host : E, args : Seq[E]) : E = {
    args match {
      case Seq() ⇒ exprUndefined
      case Seq(x) ⇒ x
      case _ ⇒
        new Expression(
          args.dropRight(1).foldRight(args.last.base)(
            (y, x) ⇒ baseBinary(y.base, x)),
          None,
          standardInvoke,
          aggSpecE(args))
    }
  }

  /** Binary member invocation. */
  private def binaryMemberInvoke(host : E, args : Seq[E]) : E = {
    args match {
      case Seq() ⇒ exprUndefined
      case Seq(x) ⇒ x
      case _ ⇒
        new Expression(
          args.tail.foldLeft(args.head.base)(
            (x, y) ⇒
              y.constValue match {
                case None ⇒ base.dynamicMember(x, y.base)
                case Some(m) ⇒ base.knownMember(x, m)
              }),
          None,
          standardInvoke,
          aggSpecE(args))
    }
  }


  /** Creates a binary operator. */
  private def binary(op : (BE, BE) ⇒ BE) : E =
    mkSpecialOperator(
      (h, a) ⇒ binaryStdInvoke(op, h, a),
      Seq.empty,
      mkFoldArgs(op))


  /** Creates a binary assignment operator. */
  private def binaryAssign(op : (BE, BE) ⇒ BE) : E =
    new Expression(
      base.exprUndefined,
      None,
      (h, a) ⇒ binaryAssignInvoke(op, h, a),
      Set.empty)


  /** Creates a "fold arguments" code. */
  private def mkFoldArgs(baseBinary : (BE, BE) ⇒ BE) : Seq[BS] = {
    val lenId = new Object
    val len = base.variable(lenId)
    val iId = new Object
    val i = base.variable(iId)
    val resId = new Object
    val res = base.variable(resId)

    Seq(
      base.defVars(Seq(
        (lenId, Some(base.knownMember(base.exprArguments, "length"))),
        (resId, Some(base.dynamicMember(base.exprArguments,
          base.literal(BigInteger.ZERO)))))),
      base.forVars(
        Seq((iId, Some(base.literal(BigInteger.ONE)))),
        Some(base.less(i, len)),
        Some(base.postfixInc(i)),
        Seq(base.exprStatement(
          base.assign(res, baseBinary(res,
            base.dynamicMember(base.exprArguments, i)))))))
  }


  /** Creates a new special expression. */
  private def mkSpecialOperator(
        invoker : (E, Seq[E]) ⇒ E,
        args : Seq[AnyRef],
        implementation : Seq[BS])
      : E = {
    val exprId = new Object

    val special = new FirstClassWrapper(exprId, args, implementation)

    new Expression(base.variable(exprId), None, invoker, Set(special))
  }


  /** Aggregates specials from expressions. */
  private def aggSpecE(args : Seq[E]) : Set[FirstClassWrapper[BS]] =
    args.foldLeft(Set.empty[FirstClassWrapper[BS]])(
      (s, a) ⇒ s ++ a.firstClassOperators)
}
