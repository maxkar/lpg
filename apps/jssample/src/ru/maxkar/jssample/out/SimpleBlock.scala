package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.lispy.parser.Input

import ru.maxkar.backend.js.model._
import ru.maxkar.backend.js.model.Model._

import ru.maxkar.scoping.simple._
import ru.maxkar.scoping.simple.Scope._

import java.io.File

import scala.collection.mutable.ArrayBuffer


/** Sequence block. */
private final class SeqBlock (stmts : Seq[BlockCompiler]) extends BlockCompiler {
  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit =
    stmts.foreach(x ⇒  x.compileStatements(ctx, cb))
}


/** Subscope block. */
private final class SubScopeBlock (
      base : LocalContextBuilder,
      peer : BlockCompiler)
    extends BlockCompiler {

  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit =
    peer.compileStatements(base.endRelated(ctx), cb)
}


/** Assignment block. */
private final class AssingVarBlock(
    variable : Symbol, expr : SExpression[BaseItem])
    extends BlockCompiler {
  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit =
    cb(assign(
      variable.resolve.asInstanceOf[LeftValue],
      ctx.exprs.compile(expr)))
}


private final class RetBlock(expr : SExpression[BaseItem])
    extends BlockCompiler {

  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit =
    cb(returns(ctx.exprs.compile(expr)))
}


private final class WhenBlock(
    cond : SExpression[BaseItem], tl : BlockCompiler)
    extends BlockCompiler {

  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit =
    cb(when(
      ctx.exprs.compile(cond), tl.compileToSeq(ctx)))
}


private final class WhileBlock(
    cond : SExpression[BaseItem], tl : BlockCompiler)
    extends BlockCompiler {

  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit =
    cb(whiles(
      ctx.exprs.compile(cond), tl.compileToSeq(ctx)))
}


private final class IfBlock(
    cond : SExpression[BaseItem], tl1 : BlockCompiler, tl2 : BlockCompiler)
    extends BlockCompiler {

  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit =
    cb(doCond(
      ctx.exprs.compile(cond),
      tl1.compileToSeq(ctx),
      tl2.compileToSeq(ctx)))
}


/** Expression block compiler. */
private class ExprBlock(expr : SExpression[BaseItem]) extends BlockCompiler {
  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit =
    cb(ctx.exprs.compile(expr))
}



/** Simple block compiler. */
object SimpleBlock {
  import CompilerUtil._

  /** Joins to an existing scope/context. Returned block will not have
   * it's own namespace.
   */
  def joinTo(
        ctx : LocalContextBuilder,
        stmts : Seq[SExpression[BaseItem]]) : BlockCompiler = {

    val blocks = new ArrayBuffer[BlockCompiler]

    stmts foreach (x ⇒  x match {
        case SList(Seq(SLeaf(BaseId("var"), _), tl@_*), _) ⇒
          tl.foreach(dcl ⇒ dcl match {
              case SLeaf(BaseId(id), _) ⇒ ctx.mkVar(id, x)
              case SList(Seq(a@SLeaf(BaseId(id), _), expr), _) ⇒
                val ide = ctx.mkVar(id, a)
                blocks += new AssingVarBlock(ide, expr)
              case _ ⇒
                ctx.trace.mailformedDeclaration(dcl)
            })
        case SList(Seq(SLeaf(BaseId("def"), _), SLeaf(BaseId(fname), _), SList(args, _), tl@_*), _) ⇒
          ctx.mkFunction(fname, x, args, tl)
        case SList(Seq(SLeaf(BaseId("ret"), _), expr), _) ⇒
          blocks += new RetBlock(expr)
        case SList(Seq(SLeaf(BaseId("do"), _), tl@_*), _) ⇒
          blocks += sub(ctx, tl)
        case SList(Seq(SLeaf(BaseId("when"), _), cond, tl@_*), _) ⇒
          blocks += new WhenBlock(cond, sub(ctx, tl))
        case SList(Seq(SLeaf(BaseId("while"), _), cond, tl@_*), _) ⇒
          blocks += new WhileBlock(cond, sub(ctx, tl))
        case SList(Seq(SLeaf(BaseId("if"), _), cond, SList(tl1, _), SList(tl2, _)), _) ⇒
          blocks += new IfBlock(cond, sub(ctx, tl1), sub(ctx, tl2))
        case _ ⇒ blocks += new ExprBlock(x)
      })

    new SeqBlock(blocks)
  }



  /** Creates a subblock with a local scope. */
  def sub(ctx : LocalContextBuilder, stmts : Seq[SExpression[BaseItem]])
      : BlockCompiler = {
    val sb = ctx.newSubBuilder
    val peer = joinTo(sb, stmts)
    new SubScopeBlock(sb, peer)
  }
}
