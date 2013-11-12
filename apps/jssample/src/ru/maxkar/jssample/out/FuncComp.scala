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

/** Function compiler access point. */
private[out] object FuncComp {
  import CompilerUtil._
  import ExprComp._

  /** Compiles a function body. */
  def compFunction(
        host : File,
        baddecl : ArrayBuffer[Message],
        scope : Scope[String, ToplevelItem],
        args : Seq[SExpression[BaseItem]],
        tl : Seq[SExpression[BaseItem]]) :  FunctionBody = {

    val root = new RootScopeBuilder(host)
    val varb = new ScopeBuilder[String, ToplevelItem]
    val labb = new ScopeBuilder[String, ToplevelItem]

    args.foreach(x ⇒  x match {
        case SLeaf(BaseId(id), _) ⇒ varb.offer(id, root.mkArg(x))
        case _ ⇒  baddecl += BadArgument(host, locOf(x))
      })

    val stmtb = SimpleBlock.joinTo(host, varb, labb, root,
      baddecl, tl)

    baddecl ++= varb.duplicates.map(x ⇒
      new DuplicateDeclaration(host, x._1, x._2.declarationHost.offset, x._3.declarationHost.offset))
    baddecl ++= labb.duplicates.map(x ⇒
      new DuplicateDeclaration(host, x._1, x._2.declarationHost.offset, x._3.declarationHost.offset))

    val vs = Scope.chain(scope, varb.scope)

    new FunctionBody(
      root.getArgs,
      root.getVars,
      root.getFuncs.map(x ⇒  (x._1,
        compFunction(host, baddecl, vs, x._2, x._3))),
      root.getLabels,
      stmtb.compileToSeq(vs, labb.scope, baddecl))
  }
}
