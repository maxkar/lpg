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

/** Compiler for the premodule. */
private[out] final class PremoduleCompiler(
    host : File, rs : Scope[String, ToplevelItem]) {

  import PremoduleCompiler._

  private val baddecl = new ArrayBuffer[Message]
  private val funcmap = new HashMap[String, FunctionBody]
  private val stmts = new ArrayBuffer[Statement]
  private val globs = new HashSet[String]

  private def locOf(x : SExpression[BaseItem]) : TextPosition = {
    val slist = x.atts.allValues(Input.textPosition)
    if (slist.isEmpty)
      null
    else
      slist.iterator.next
  }


  /** Resolves an identifier. */
  private def resolveId(scope : Scope[String, ToplevelItem],
        id : String, loc : TextPosition)
      : Expression = {
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



  /** Compiles an expression. */
  private def compileExpr(scope : Scope[String, ToplevelItem], item : SExpression[BaseItem]) : Expression = {
    item match {
      case SLeaf(BaseInteger(x), _) ⇒
        literal(x)
      case SLeaf(BaseFloating(x, y), _) ⇒
        literal(x, y)
      case SLeaf(BaseString(x), _) ⇒
        literal(x)
      case SLeaf(BaseId(x), _) ⇒
        resolveId(scope, x, locOf(item))
      case SList(Seq(SLeaf(BaseId("fun"), _),
          SList(args, _),
          body@_*), _) ⇒
        val fb = compFunction(scope, args, body)
        anonfun(fb.args, fb.vars, fb.funcs, fb.labels, fb.stmt)
      case SList(Seq(SLeaf(BaseId("if"), _), c, l, r), _) ⇒
        cond(compileExpr(scope, c), compileExpr(scope, l), compileExpr(scope, r))
      case SList(Seq(SLeaf(BaseId(x), _), a, b), _) if BINARY_OPS.contains(x) ⇒
        BINARY_OPS(x)(compileExpr(scope, a), compileExpr(scope, b))
      case SList(Seq(SLeaf(BaseId(x), _), a, b), _) if BINARY_ASSIGN.contains(x) ⇒
        val lv = compileExpr(scope, a)
        if (!lv.isInstanceOf[LeftValue]) {
          baddecl += UnassignableExpression(host, locOf(item))
          Model.failure
        } else
          BINARY_ASSIGN(x)(lv.asInstanceOf[LeftValue], compileExpr(scope, b))
      case SList(Seq(SLeaf(BaseId(x), _), a), _) if UNARY_OPS.contains(x) ⇒
        UNARY_OPS(x)(compileExpr(scope, a))
      case SList(Seq(SLeaf(BaseId(x), _), a), _) if UNARY_ASSIGN.contains(x) ⇒
        val lv = compileExpr(scope, a)
        if (!lv.isInstanceOf[LeftValue]) {
          baddecl += UnassignableExpression(host, locOf(item))
          Model.failure
        } else
          UNARY_ASSIGN(x)(lv.asInstanceOf[LeftValue])
      case SList(Seq(hd, tl@_*), _) ⇒
        val x = compileExpr(scope, hd)
        if (!x.isInstanceOf[NonprimitiveExpression]) {
          baddecl += UncallableExpression(host, locOf(item))
          tl.foreach(compileExpr(scope, _))
          Model.failure
        }
        else
          call(x.asInstanceOf[NonprimitiveExpression], tl.map(compileExpr(scope, _)) :_*)
      case _ ⇒
        baddecl += BadExpression(host, locOf(item))
        Model.failure
    }
  }


  private def acceptVar(item : SExpression[BaseItem]) : Unit = {
    item match {
      case SLeaf(BaseId(x), _) ⇒
        globs += x
      case SList(Seq(a@SLeaf(BaseId(x), _), iv), _) ⇒
        globs += x
        stmts += assign(global(x), compileExpr(rs, iv))
      case _ ⇒ ()
    }
  }


  /** Compiles a function body. */
  private def compFunction(
        scope : Scope[String, ToplevelItem],
        args : Seq[SExpression[BaseItem]],
        tl : Seq[SExpression[BaseItem]]) :  FunctionBody = {

    val locals = new ArrayBuffer[ToplevelItem]
    val allLocals = new ScopeBuilder[String, ToplevelItem]
    val labels = new ScopeBuilder[String, ToplevelItem]
    val elts = new ArrayBuffer[(Scope[String, ToplevelItem], Scope[String, ToplevelItem], Statement ⇒  Unit) ⇒  Unit]
    val funcs = new ArrayBuffer[(ToplevelItem, (Scope[String, ToplevelItem]) ⇒  FunctionBody)]
    val argnames = new ArrayBuffer[String]

    args.foreach(a ⇒ a match {
      case SLeaf(BaseId(x), _) ⇒
        allLocals.offer(x, new TILvar(new ModuleHost(host, locOf(a))))
        argnames += x
      case _ ⇒ baddecl += BadArgument(host, locOf(a))
    })

    def compstmt(x : SExpression[BaseItem]) : Unit = {
      x match {
        case SList(Seq(SLeaf(BaseId("var"), _), tl@_*), _) ⇒
          tl.foreach(dcl ⇒ dcl match {
              case SLeaf(BaseId(x), _) ⇒
                val vv = new TILvar(new ModuleHost(host, locOf(dcl)))
                allLocals.offer(x, vv)
                locals += vv
              case SList(Seq(SLeaf(BaseId(x), _), expr), _) ⇒
                val vv = new TILvar(new ModuleHost(host, locOf(dcl)))
                allLocals.offer(x, vv)
                locals += vv
                elts += ((a, b, w) ⇒  w(assign(a.lookup(x).head.resolve(a).asInstanceOf[LeftValue], compileExpr(a, expr))))
              case _ ⇒
                baddecl += BadDeclaration(host, locOf(dcl))
            })
        case SList(Seq(SLeaf(BaseId("def"), _), SLeaf(BaseId(fname), _), SList(args, _), tl@_*), _) ⇒
          val vv = new TILvar(new ModuleHost(host, locOf(x)))
          allLocals.offer(fname, vv)
          funcs += ((vv, sc ⇒  compFunction(sc, args, tl)))
        case SList(Seq(SLeaf(BaseId("ret"), _), expr), _) ⇒
          elts += ((x, y, w) ⇒ w(returns(compileExpr(x, expr))))
        case _ ⇒  elts += ((s1, s2, cb) ⇒  cb(compileExpr(s1, x)))
      }
    }

    tl.foreach(compstmt)

    allLocals.duplicates.foreach(x ⇒
      baddecl += DuplicateDeclaration(host, x._1,  x._2.declarationHost.offset, x._3.declarationHost.offset))
    labels.duplicates.foreach(x ⇒
      baddecl += DuplicateDeclaration(host, x._1,  x._2.declarationHost.offset, x._3.declarationHost.offset))

    val varscope = Scope.chain(scope, allLocals.scope)
    val lscope = labels.scope

    val fbody = new ArrayBuffer[Statement]
    elts.foreach(x ⇒  x(varscope, lscope, fbody.+=))

    new FunctionBody(
      argnames.map(x ⇒  varscope.lookup(x).head),
      locals,
      funcs.map(x ⇒  (x._1, x._2(varscope))),
      labels.entries.values.toSeq,
      fbody)
  }


  private def acceptFunction(item : Seq[SExpression[BaseItem]]) : Unit = {
    item match {
      case Seq(
          hd@SLeaf(BaseId(x), _),
          SList(args, _),
          tail@_*) ⇒
        val fb = compFunction(rs, args, tail)
        funcmap.put(x, fb)
      case _ ⇒ ()
    }
  }


  private def acceptDef(item : SExpression[BaseItem]) : Unit = {
    val hleaf = item.unHeadLeaf
    if (hleaf == null)
      return

    val idn = hleaf.unId
    if (idn == null)
      return

    idn match {
      case "var" ⇒ item.tail.foreach(acceptVar)
      case "def" ⇒ acceptFunction(item.tail)
      case _ ⇒ ()
    }
  }



  def acceptTop(item : SExpression[BaseItem]) : Unit = {
    item match {
      case SList(items, _) ⇒
        items.foreach(acceptDef)
      case _ ⇒ ()
    }
  }


  def end() :
      ((Set[String], Seq[(String, FunctionBody)], Seq[Statement]), Seq[Message]) =
    ((globs.toSet, funcmap.toSeq, stmts), baddecl)

}


/** Premodule items. */
private object PremoduleCompiler {
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
}
