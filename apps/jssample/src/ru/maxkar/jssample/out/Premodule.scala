package ru.maxkar.jssample.out

import ru.maxkar.jssample.msg.HostTrace

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.TextPosition

import ru.maxkar.jssample.msg.HostTrace
import ru.maxkar.jssample.doc._

import ru.maxkar.backend.js.model._

import ru.maxkar.scoping.simple._

import java.io.File

import scala.collection.mutable.ArrayBuffer


/**
 * Class, which can be turned into a module by
 * providing an "external context"
 */
final class Premodule(
    val id : Seq[String],
    val globals : Seq[(String, Symbol)],
    val publicScope : Scope[String, Symbol],
    val doc : Option[DocBody],
    val varDoc : Seq[VarDoc],
    val funDoc : Seq[FunDoc],
    localScope : Scope[String, Symbol],
    varInitializers : Seq[(Symbol, SExpression[BaseItem])],
    allFunctions : Seq[(Symbol, Seq[SExpression[BaseItem]], Seq[SExpression[BaseItem]])],
    allVars : Seq[Symbol],
    defroot : SExpression[BaseItem],
    val module : File) {

  def compile(rs : Scope[String, Symbol], mods : Scope[String, Premodule], trace : HostTrace)
      : ((Set[Symbol], Seq[(Symbol, FunctionBody)], Seq[Statement])) = {

    val modScope = Scope.chain(rs, localScope)
    val baseScope = new SymbolScope(rs, localScope, mods, trace).forNested(defroot)
    val symScope = baseScope.subscope(localScope)

    val mc = new ExprComp(module, trace, symScope)

    val stmts = varInitializers.map(x ⇒
      Model.assign(x._1.resolve.asInstanceOf[LeftValue], mc.compile(x._2)))

    (allVars.toSet,
      allFunctions.map(x ⇒
        (x._1, FuncComp.compFunction(module, symScope, x._2, x._3, trace))),
      stmts)
  }
}



/** Utilities for the premodule. */
final object Premodule {

  /** Precompiles items into a new premodule. Returns a new
   * module, list of duplicate declarations and list of bad declarations. */
  def precompile(trace : HostTrace, id : Seq[String], module : File, elts : SList[BaseItem]) : Premodule = {
    val pmb = new PremoduleBuilder(trace, module, id)
    elts.items.foreach(pmb.acceptDef)
    pmb.end(elts)
  }
}
