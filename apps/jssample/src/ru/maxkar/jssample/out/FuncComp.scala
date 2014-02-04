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

  /** Compiles to a body. */
  def compToBody(
        host : java.io.File,
        scope : SymbolScope,
        isVaarg : Boolean,
        args : Seq[SExpression[BaseItem]],
        tl : Seq[SExpression[BaseItem]],
        trace : HostTrace) : FunctionBody = {
    val t = compFunction(host, scope, isVaarg, args, tl, trace)
    Model.mkFunctionBody(t._1, t._2, t._3, t._4, t._5)
  }

  /** Compiles a function body. */
  def compFunction(
        host : java.io.File,
        scope : SymbolScope,
        isVaarg : Boolean,
        args : Seq[SExpression[BaseItem]],
        tl : Seq[SExpression[BaseItem]],
        trace : HostTrace) : (Seq[AnyRef], Seq[AnyRef], Seq[(AnyRef, FunctionBody)], Seq[AnyRef], Seq[Statement]) = {


    val root = new RootScopeBuilder(host)
    val lcb = new LocalContextBuilder(trace, host, root)

    val functab = new ArrayBuffer[(AnyRef, FunctionBody)]

    var reallyVararg = isVaarg && !args.isEmpty

    var funArgs =
      if (reallyVararg)
        args.dropRight(1)
      else
        args

    funArgs.foreach(x ⇒  x match {
        case SLeaf(BaseId(id), _) ⇒ lcb.mkArg(id, x)
        case _ ⇒  trace.mailformedDeclaration(x)
      })

    if (reallyVararg)
      args.last match {
        case x@SLeaf(BaseId(id), _) ⇒ lcb.mkVar(id, x)
        case x ⇒  trace.mailformedDeclaration(x)
      }

    val stmtb = SimpleBlock.joinTo(lcb, tl)

    val locCtx = lcb.end(scope, (x, y) ⇒ functab += ((x, y)))
    val vs = locCtx.variables

    val vaargsinit : Seq[Statement] =
      if (!reallyVararg)
        Seq.empty
      else
        args.last match {
         case a@SLeaf(BaseId(id), _) ⇒
           (vs.lookup(id, a), vs.lookup("Array", a)) match {
             case (Some(v), Some(a)) ⇒
               Seq(assign(v.resolve.asInstanceOf[LeftValue],
                 call(
                   member(member(member(a.resolve, literal("prototype")),
                     literal("slice")), literal("call")),
                   arguments,
                   literal(args.size - 1))))
             case _ ⇒ Seq.empty
           }
         case _ ⇒ Seq.empty
        }

    (
      root.getArgs,
      root.getVars,
      functab,
      root.getLabels,
      vaargsinit ++ stmtb.compileToSeq(locCtx))
  }
}
