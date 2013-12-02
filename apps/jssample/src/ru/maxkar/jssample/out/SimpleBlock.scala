package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.jssample.att.Vararg
import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.lispy.parser.Input

import ru.maxkar.backend.js.model._
import ru.maxkar.backend.js.model.Model._

import ru.maxkar.scoping.simple._
import ru.maxkar.scoping.simple.Scope._

import ru.maxkar.jssample.msg.HostTrace

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
      variable.resolve,
      ctx.exprs.compile(expr)))
}


private final class RetBlock(expr : SExpression[BaseItem])
    extends BlockCompiler {

  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit =
    cb(returns(ctx.exprs.compile(expr)))
}


private final class RetNoneBlock() extends BlockCompiler {

  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit =
    cb(returnNothing)
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


/** For block compiler. */
private class ForArray(idxVar : Symbol, arrVar : Symbol, valVar : Symbol,
      expr : SExpression[BaseItem], tl : BlockCompiler) extends BlockCompiler {

  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit = {
    cb(assign(idxVar.resolve, literal(0)))
    cb(assign(arrVar.resolve, ctx.exprs.compile(expr)))
    val updExpr = prefixInc(idxVar.resolve)
    val loopCond = less(idxVar.resolve, member(arrVar.resolve, literal("length")))

    val loadItem = assign(valVar.resolve, member(arrVar.resolve, idxVar.resolve))

    cb(whileWithIterupdate(loopCond, updExpr, loadItem +: tl.compileToSeq(ctx)))
  }
}


/** For-key block compiler. */
private class ForKey(valVar : Symbol,
      expr : SExpression[BaseItem], tl : BlockCompiler) extends BlockCompiler {

  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit = {
    cb(forIn(valVar.resolve, ctx.exprs.compile(expr), tl.compileToSeq(ctx)))
  }
}


/** Catch block compiler. */
private class CatchBlock(exnVar : Symbol, exn: BlockCompiler, body : BlockCompiler)
    extends BlockCompiler {

  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit =
    cb(tryCatch(body.compileToSeq(ctx), exnVar, exn.compileToSeq(ctx)))
}


/** Case block compiler. */
private class CaseBlock(
      expr : SExpression[BaseItem],
      cases : Seq[(Seq[SExpression[BaseItem]], BlockCompiler)],
      deflt : Option[BlockCompiler])
    extends BlockCompiler {

  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit = {
    cb(switchof(
        ctx.exprs.compile(expr),
        cases.map(x ⇒ (
            x._1.map(ctx.exprs.compile),
            x._2.compileToSeq(ctx))),
        deflt.map(x ⇒ x.compileToSeq(ctx))))
  }
}


/** Function block compiler. */
private class FuncBlock (
      key : Symbol,
      host : java.io.File,
      isVaarg : Boolean,
      args : Seq[SExpression[BaseItem]],
      tl : Seq[SExpression[BaseItem]],
      trace : HostTrace)
    extends BlockCompiler {

  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit = {
    ctx.funcImpl(key, FuncComp.compFunction(host, ctx.variables,
      isVaarg, args, tl, trace))
  }
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
        case SList(Seq(dfn@SLeaf(BaseId("def"), _), SLeaf(BaseId(fname), _), SList(args, _), tl@_*), _) ⇒
          val vaarg = !dfn.atts.allValues(Vararg.ATTR).isEmpty
          if (vaarg && args.isEmpty)
            ctx.trace.noArgVararg(x)
          var fkey = ctx.mkAutoVar(fname, dfn)
          blocks += new FuncBlock(fkey, ctx.host, vaarg, args, tl, ctx.trace)
        case SList(Seq(SLeaf(BaseId("ret"), _), expr), _) ⇒
          blocks += new RetBlock(expr)
        case SList(Seq(SLeaf(BaseId("ret"), _)), _) ⇒
          blocks += new RetNoneBlock
        case SList(Seq(SLeaf(BaseId("do"), _), tl@_*), _) ⇒
          blocks += sub(ctx, tl)
        case SList(Seq(SLeaf(BaseId("when"), _), cond, tl@_*), _) ⇒
          blocks += new WhenBlock(cond, sub(ctx, tl))
        case SList(Seq(SLeaf(BaseId("while"), _), cond, tl@_*), _) ⇒
          blocks += new WhileBlock(cond, sub(ctx, tl))
        case SList(Seq(SLeaf(BaseId("if"), _), cond, SList(tl1, _), SList(tl2, _)), _) ⇒
          blocks += new IfBlock(cond, sub(ctx, tl1), sub(ctx, tl2))
        case SList(Seq(SLeaf(BaseId("for-array"), _),
            iid@SLeaf(BaseId(itr), _), init, tl@_*),_) ⇒
          val iscope = ctx.newSubBuilder
          val vr = iscope.mkVar(itr, iid)
          val lv = iscope.mkAnonVar(x)
          val av = iscope.mkAnonVar(x)
          blocks +=
            new SubScopeBlock(iscope,
              new ForArray(lv, av, vr, init, sub(ctx, tl)))
        case SList(Seq(SLeaf(BaseId("for-key"), _),
            iid@SLeaf(BaseId(itr), _), init, tl@_*),_) ⇒
          val iscope = ctx.newSubBuilder
          val vr = iscope.mkVar(itr, iid)
          blocks +=
            new SubScopeBlock(iscope,
              new ForKey(vr, init, sub(ctx, tl)))
        case SList(Seq(SLeaf(BaseId("on-exn"), _),
              exnId@SLeaf(BaseId(exn), _),
              SList(exnInst, _), tl@_*), _) ⇒
          val iscope = ctx.newSubBuilder
          val evar = iscope.mkAutoVar(exn, exnId)
          blocks +=
            new CatchBlock(evar,
              new SubScopeBlock(iscope,
                sub(iscope, exnInst)),
              sub(ctx, tl))
        case SList(Seq(SLeaf(BaseId("case"), _), expr, tl@_*), _) ⇒
          var dflt : Option[(SExpression[BaseItem], BlockCompiler)] = None
          var cases = new ArrayBuffer[(Seq[SExpression[BaseItem]], BlockCompiler)]

          def mkSub(items : Seq[SExpression[BaseItem]]) : BlockCompiler = {
            val iscope = ctx.newSubBuilder
            new SubScopeBlock(iscope, sub(iscope, items))
          }

          tl.foreach(x ⇒  x match {
            case a@SList(Seq(SList(Seq(), _), commands@_*), _) ⇒
              dflt match {
                case None ⇒
                  dflt = Some((a, mkSub(commands)))
                case Some((p, _)) ⇒
                  ctx.trace.duplicateDefaultCase(p, a)
              }
            case SList(Seq(SList(exprs, _), commands@_*), _) ⇒
              cases += ((exprs, mkSub(commands)))
            case _ ⇒
              ctx.trace.mailformedCaseClause(x)
          })

        blocks += new CaseBlock(expr, cases, dflt.map(_._2))
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
