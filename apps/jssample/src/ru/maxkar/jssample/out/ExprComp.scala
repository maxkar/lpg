package ru.maxkar.jssample.out

import ru.maxkar.jssample.msg.HostTrace

import ru.maxkar.jssample.ns._
import ru.maxkar.jssample.att.Vararg
import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.lispy.parser.Input

import ru.maxkar.backend.js.model._
import ru.maxkar.backend.js.model.Model._

import ru.maxkar.scoping.simple._

import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.JavaConversions._


/** Expression compiler closure. */
private[out] class ExprComp(
    host : java.io.File,
    trace : HostTrace,
    scope : SymbolScope) {

  import ExprComp._
  import CompilerUtil._


  /** Resolves a base (primitive) identifier. */
  private def resolveBaseId(id : String, item : SExpression[BaseItem]) : Expression = {
    scope.lookup(id, item) match {
      case None ⇒ Model.failure
      case Some(x) ⇒ x.resolve
    }
  }


  /** Resolves an identifier. */
  private def resolveId(id : String, item : SExpression[BaseItem]) : Expression = {
    val guess = scope.lookupAll(id, item)

    if (guess.isEmpty && !id.startsWith(".") && id.contains(".")) {
      val parts = id.split("\\.")
      parts.tail.map(Model.literal).foldLeft(resolveBaseId(parts.head, item))(Model.member)
    } else
      resolveBaseId(id, item)
  }



  /** Compiles an expression. */
  def compile(item : SExpression[BaseItem]) : Expression = {
    item match {
      case SLeaf(BaseInteger(x), _) ⇒ literal(x)
      case SLeaf(BaseFloating(x, y), _) ⇒ literal(x, y)
      case SLeaf(BaseString(x), _) ⇒ literal(x)
      case SLeaf(BaseId("null"), _) ⇒ exprNull
      case SLeaf(BaseId(x), _) ⇒ resolveId(x, item)
      case SList(Seq(fdn@SLeaf(BaseId("fun"), _),
          SList(args, _),
          body@_*), _) ⇒
        val isvaarg = !fdn.atts.allValues(Vararg.ATTR).isEmpty
        if (isvaarg && args.isEmpty)
          trace.noArgVararg(item)
        val fb = FuncComp.compFunction(host, scope, isvaarg, args, body, trace)
        anonfun(fb.args, fb.vars, fb.funcs, fb.labels, fb.stmt)
      case SList(Seq(SLeaf(BaseId("if"), _), c, l, r), _) ⇒
        cond(compile(c), compile(l), compile(r))
      case SList(Seq(SLeaf(BaseId(x), _), a, b), _) if BINARY_OPS.contains(x) ⇒
        BINARY_OPS(x)(compile(a), compile(b))
      case SList(Seq(SLeaf(BaseId(x), _), a, b), _) if BINARY_ASSIGN.contains(x) ⇒
        val lv = compile(a)
        if (!lv.isInstanceOf[LeftValue]) {
          trace.unassignableExpression(item)
          Model.failure
        } else
          BINARY_ASSIGN(x)(lv.asInstanceOf[LeftValue], compile(b))
      case SList(Seq(SLeaf(BaseId(x), _), a), _) if UNARY_OPS.contains(x) ⇒
        UNARY_OPS(x)(compile(a))
      case SList(Seq(SLeaf(BaseId(x), _), a), _) if UNARY_ASSIGN.contains(x) ⇒
        val lv = compile(a)
        if (!lv.isInstanceOf[LeftValue]) {
          trace.unassignableExpression(item)
          Model.failure
        } else
          UNARY_ASSIGN(x)(lv.asInstanceOf[LeftValue])
      case SList(Seq(SLeaf(BaseId(x), item), tl), _) if x.startsWith(".") ⇒
        val path = x.split("\\.")
        var agg = compile(tl)
        path.tail.map(Model.literal).foldLeft(compile(tl))(Model.member)
      case SList(Seq(hd, tl@_*), _) ⇒
        val x = compile(hd)
        if (!x.isInstanceOf[NonprimitiveExpression]) {
          trace.uncallableExpression(item)
          tl.foreach(compile)
          Model.failure
        }
        else
          call(x.asInstanceOf[NonprimitiveExpression], tl.map(compile) :_*)
      case _ ⇒
        trace.uncallableExpression(item)
        Model.failure
    }
  }
}


/** Expression compiler. */
private[out] object ExprComp {
  /** Binary operator definitions. */
  private val BINARY_OPS : Map[String, (Expression, Expression) ⇒ Expression] =
    Map(
      "." → Model.member,
      "*" → Model.mul,
      "/" → Model.div,
      "%" → Model.rem,
      "+" → Model.add,
      "-" → Model.sub,
      "<<" → Model.shl,
      ">>" → Model.sshr,
      ">>>" → Model.ushr,
      "<" → Model.less,
      "<=" → Model.lessEq,
      ">" → Model.greater,
      ">=" → Model.greaterEq,
      "in" → Model.isIn,
      "instanceof" → Model.testInstanceOf,
      "==" → Model.equalsTo,
      "!=" → Model.notEquals,
      "===" → Model.strictEquals,
      "!==" → Model.strictNotEquals,
      "&" → Model.bitAnd,
      "^" → Model.bitXor,
      "|" → Model.bitOr,
      "&&" → Model.boolAnd,
      "||" → Model.boolOr
    )

  /** Binary assign operator definitions. */
  private val BINARY_ASSIGN : Map[String, (LeftValue, Expression) ⇒ Expression] =
    Map(
      "=" → Model.assign,
      "+=" → Model.inplaceAdd,
      "-=" → Model.inplaceSub,
      "*=" → Model.inplaceMul,
      "/=" → Model.inplaceDiv,
      "%=" → Model.inplaceRem,
      "<<=" → Model.inplaceShl,
      ">>=" → Model.inplaceSshr,
      ">>>=" → Model.inplaceUshr,
      "&=" → Model.inplaceBitAnd,
      "|=" → Model.inplaceBitOr,
      "^=" → Model.inplaceBitXor
    )


  /** Simple unary operations. */
  private val UNARY_OPS : Map[String, Expression ⇒  Expression] =
    Map(
      "!" → Model.boolNot,
      "~" → Model.bitNot,
      "-" → Model.neg,
      "typeof" → Model.typeof
    )

  /** Unary operations, which changes it's value. */
  private val UNARY_ASSIGN : Map[String, LeftValue ⇒  Expression] =
    Map(
      "++_" → Model.prefixInc,
      "--_" → Model.prefixDec,
      "_++" → Model.postfixInc,
      "_--" → Model.postfixDec,
      "delete" → Model.delete
    )
}
