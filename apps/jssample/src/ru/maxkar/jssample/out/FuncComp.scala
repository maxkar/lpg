package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.lispy.parser.Input

import ru.maxkar.backend.js.model._
import ru.maxkar.backend.js.model.Model._

import ru.maxkar.scoping.simple._
import ru.maxkar.jssample.msg.HostTrace


import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.JavaConversions._

/** Function compiler access point. */
private[out] object FuncComp {
  import CompilerUtil._
  import ExprComp._

  /** Compiles a function body. */
  def compFunction(
        host : java.io.File,
        scope : SymbolScope,
        args : Seq[SExpression[BaseItem]],
        tl : Seq[SExpression[BaseItem]],
        trace : HostTrace) :  FunctionBody = {


    val root = new RootScopeBuilder(host)
    val lcb = new LocalContextBuilder(trace, host, root)

    args.foreach(x ⇒  x match {
        case SLeaf(BaseId(id), _) ⇒ lcb.mkArg(id, x)
        case _ ⇒  trace.mailformedDeclaration(x)
      })

    val stmtb = SimpleBlock.joinTo(lcb, tl)

    val locCtx = lcb.end(scope)
    val vs = locCtx.variables

    new FunctionBody(
      root.getArgs,
      root.getVars,
      root.getFuncs.map(x ⇒  (x._1,
        compFunction(host, vs, x._2, x._3, trace))),
      root.getLabels,
      stmtb.compileToSeq(locCtx))
  }
}
