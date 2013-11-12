package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
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
private class ExprComp(
    host : File,
    baddecl : ArrayBuffer[Message],
    scope : Scope[String, ToplevelItem]) {

  import ExprComp._
  import CompilerUtil._


  /** Resolves an identifier. */
  private def resolveId(id : String, loc : TextPosition) : Expression = {
    val  guess = scope.lookup(id)
    if (guess.isEmpty) {
      baddecl += UndeclaredIdentifier(host, loc)
      Model.failure
    } else if (guess.size > 1) {
      baddecl += AmbigiousIdentifier(host, loc,
          guess.toSeq.map(_.declarationHost))
      Model.failure
    } else
      guess.head.resolve(scope)
  }


  def compile(item : SExpression[BaseItem]) : Expression = {
    item match {
      case SLeaf(BaseInteger(x), _) ⇒ literal(x)
      case SLeaf(BaseFloating(x, y), _) ⇒ literal(x, y)
      case SLeaf(BaseString(x), _) ⇒ literal(x)
      case SLeaf(BaseId(x), _) ⇒
        resolveId(x, locOf(item))
      case SList(Seq(SLeaf(BaseId("fun"), _),
          SList(args, _),
          body@_*), _) ⇒
        val fb = FuncComp.compFunction(host, baddecl, scope, args, body)
        anonfun(fb.args, fb.vars, fb.funcs, fb.labels, fb.stmt)
      case SList(Seq(SLeaf(BaseId("if"), _), c, l, r), _) ⇒
        cond(compile(c), compile(l), compile(r))
      case SList(Seq(SLeaf(BaseId(x), _), a, b), _) if BINARY_OPS.contains(x) ⇒
        BINARY_OPS(x)(compile(a), compile(b))
      case SList(Seq(SLeaf(BaseId(x), _), a, b), _) if BINARY_ASSIGN.contains(x) ⇒
        val lv = compile(a)
        if (!lv.isInstanceOf[LeftValue]) {
          baddecl += UnassignableExpression(host, locOf(item))
          Model.failure
        } else
          BINARY_ASSIGN(x)(lv.asInstanceOf[LeftValue], compile(b))
      case SList(Seq(SLeaf(BaseId(x), _), a), _) if UNARY_OPS.contains(x) ⇒
        UNARY_OPS(x)(compile(a))
      case SList(Seq(SLeaf(BaseId(x), _), a), _) if UNARY_ASSIGN.contains(x) ⇒
        val lv = compile(a)
        if (!lv.isInstanceOf[LeftValue]) {
          baddecl += UnassignableExpression(host, locOf(item))
          Model.failure
        } else
          UNARY_ASSIGN(x)(lv.asInstanceOf[LeftValue])
      case SList(Seq(hd, tl@_*), _) ⇒
        val x = compile(hd)
        if (!x.isInstanceOf[NonprimitiveExpression]) {
          baddecl += UncallableExpression(host, locOf(item))
          tl.foreach(compile)
          Model.failure
        }
        else
          call(x.asInstanceOf[NonprimitiveExpression], tl.map(compile) :_*)
      case _ ⇒
        baddecl += BadExpression(host, locOf(item))
        Model.failure
    }
  }
}


/** Expression block compiler. */
private class ExprBlock(host : File, expr : SExpression[BaseItem]) extends BlockCompiler {
  def compileStatements(
      vars : Scope[String, ToplevelItem],
      labels : Scope[String, ToplevelItem],
      baddecl : ArrayBuffer[Message],
      cb : Statement ⇒  Unit) : Unit =
    cb(ExprComp.compileExpr(host, baddecl, vars, expr))
}


/** Expression compiler. */
private[out] object ExprComp {
  /** Binary operator definitions. */
  private val BINARY_OPS : Map[String, (Expression, Expression) ⇒ Expression] =
    Map(
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


  def compileExpr(host : File,
        baddecl : ArrayBuffer[Message],
        scope : Scope[String, ToplevelItem],
        item : SExpression[BaseItem]) : Expression = {
    new ExprComp(host, baddecl, scope).compile(item)
  }


  def compileBlock(host : File, item : SExpression[BaseItem]) : BlockCompiler =
    new ExprBlock(host, item)
}
