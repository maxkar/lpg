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
  def compileStatements(
      topvars : Scope[String, ToplevelItem],
      toplabels : Scope[String, ToplevelItem],
      baddecl : ArrayBuffer[Message],
      cb : Statement ⇒  Unit) : Unit = {
    stmts.foreach(x ⇒  x.compileStatements(topvars, toplabels, baddecl, cb))
  }
}


/** Subscope block. */
private final class SubScopeBlock (
      vars : Scope[String, ToplevelItem],
      labels : Scope[String, ToplevelItem],
      peer : BlockCompiler)
    extends BlockCompiler {

  def compileStatements(
      topvars : Scope[String, ToplevelItem],
      toplabels : Scope[String, ToplevelItem],
      baddecl : ArrayBuffer[Message],
      cb : Statement ⇒  Unit) : Unit =
    peer.compileStatements(chain(topvars, vars), chain(toplabels, labels), baddecl, cb)
}


private final class AssingVarBlock(
    host : File, varble : String, expr : SExpression[BaseItem])
    extends BlockCompiler {
  def compileStatements(
      vars : Scope[String, ToplevelItem],
      labels : Scope[String, ToplevelItem],
      baddecl : ArrayBuffer[Message],
      cb : Statement ⇒  Unit) : Unit = {

    cb(assign(
      vars.lookup(varble).head.resolve(vars).asInstanceOf[LeftValue],
      ExprComp.compileExpr(host, baddecl, vars, expr)))
  }
}


private final class RetBlock(
    host : File, expr : SExpression[BaseItem])
    extends BlockCompiler {

  def compileStatements(
      vars : Scope[String, ToplevelItem],
      labels : Scope[String, ToplevelItem],
      baddecl : ArrayBuffer[Message],
      cb : Statement ⇒  Unit) : Unit = {
    cb(returns(ExprComp.compileExpr(host, baddecl, vars, expr)))
  }
}


private final class WhenBlock(
    host : File, cond : SExpression[BaseItem], tl : BlockCompiler)
    extends BlockCompiler {

  def compileStatements(
      vars : Scope[String, ToplevelItem],
      labels : Scope[String, ToplevelItem],
      baddecl : ArrayBuffer[Message],
      cb : Statement ⇒  Unit) : Unit = {
    cb(when(
      ExprComp.compileExpr(host, baddecl, vars, cond),
      tl.compileToSeq(vars, labels, baddecl)))
  }
}


private final class WhileBlock(
    host : File, cond : SExpression[BaseItem], tl : BlockCompiler)
    extends BlockCompiler {

  def compileStatements(
      vars : Scope[String, ToplevelItem],
      labels : Scope[String, ToplevelItem],
      baddecl : ArrayBuffer[Message],
      cb : Statement ⇒  Unit) : Unit = {
    cb(whiles(
      ExprComp.compileExpr(host, baddecl, vars, cond),
      tl.compileToSeq(vars, labels, baddecl)))
  }
}


private final class IfBlock(
    host : File, cond : SExpression[BaseItem], tl1 : BlockCompiler, tl2 : BlockCompiler)
    extends BlockCompiler {

  def compileStatements(
      vars : Scope[String, ToplevelItem],
      labels : Scope[String, ToplevelItem],
      baddecl : ArrayBuffer[Message],
      cb : Statement ⇒  Unit) : Unit = {
    cb(doCond(
      ExprComp.compileExpr(host, baddecl, vars, cond),
      tl1.compileToSeq(vars, labels, baddecl),
      tl2.compileToSeq(vars, labels, baddecl)))
  }
}



/** Simple block compiler. */
object SimpleBlock {
  import CompilerUtil._

  /** Joins to an existing scope/context. Returned block will not have
   * it's own namespace.
   */
  def joinTo(
        host : File,
        vars : ScopeBuilder[String, ToplevelItem],
        labels : ScopeBuilder[String, ToplevelItem],
        root : RootScopeBuilder,
        baddecl : ArrayBuffer[Message],
        stmts : Seq[SExpression[BaseItem]]) : BlockCompiler = {

    val blocks = new ArrayBuffer[BlockCompiler]

    stmts foreach (x ⇒  x match {
        case SList(Seq(SLeaf(BaseId("var"), _), tl@_*), _) ⇒
          tl.foreach(dcl ⇒ dcl match {
              case SLeaf(BaseId(x), _) ⇒
                vars.offer(x, root.mkVar(dcl))
              case SList(Seq(a@SLeaf(BaseId(x), _), expr), _) ⇒
                vars.offer(x, root.mkVar(a))
                blocks += new AssingVarBlock(host, x, expr)
              case _ ⇒
                baddecl += BadDeclaration(host, locOf(dcl))
            })
        case SList(Seq(SLeaf(BaseId("def"), _), SLeaf(BaseId(fname), _), SList(args, _), tl@_*), _) ⇒
          vars.offer(fname, root.mkFunction(x, args, tl))
        case SList(Seq(SLeaf(BaseId("ret"), _), expr), _) ⇒
          blocks += new RetBlock(host, expr)
        case SList(Seq(SLeaf(BaseId("do"), _), tl@_*), _) ⇒
          blocks += sub(host, root, baddecl, tl)
        case SList(Seq(SLeaf(BaseId("when"), _), cond, tl@_*), _) ⇒
          blocks += new WhenBlock(host, cond, sub(host, root, baddecl, tl))
        case SList(Seq(SLeaf(BaseId("while"), _), cond, tl@_*), _) ⇒
          blocks += new WhileBlock(host, cond, sub(host, root, baddecl, tl))
        case SList(Seq(SLeaf(BaseId("if"), _), cond, SList(tl1, _), SList(tl2, _)), _) ⇒
          blocks += new IfBlock(host, cond,
            sub(host, root, baddecl, tl1),
            sub(host, root, baddecl, tl2))
        case _ ⇒ blocks += ExprComp.compileBlock(host, x)
      })

    new SeqBlock(blocks)
  }



  /** Creates a subblock with a local scope. */
  def sub(host : File, root : RootScopeBuilder,
        baddecl : ArrayBuffer[Message],
        stmts : Seq[SExpression[BaseItem]]) : BlockCompiler = {

    val varb = new ScopeBuilder[String, ToplevelItem]
    val labb = new ScopeBuilder[String, ToplevelItem]
    val peer = joinTo(host, varb, labb, root, baddecl, stmts)

    baddecl ++= varb.duplicates.map(x ⇒
      new DuplicateDeclaration(host, x._1, x._2.declarationHost.offset, x._3.declarationHost.offset))
    baddecl ++= labb.duplicates.map(x ⇒
      new DuplicateDeclaration(host, x._1, x._2.declarationHost.offset, x._3.declarationHost.offset))

    new SubScopeBlock(varb.scope, labb.scope, peer)
  }
}
