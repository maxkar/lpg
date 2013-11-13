package ru.maxkar.jssample.out

import ru.maxkar.jssample.msg.HostTrace

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.TextPosition

import ru.maxkar.jssample.msg.HostTrace

import ru.maxkar.backend.js.model._

import ru.maxkar.scoping.simple._

import java.io.File

import scala.collection.mutable.ArrayBuffer


/**
 * Class, which can be turned into a module by
 * providing an "external context"
 */
final class Premodule(
    val globals : Seq[(String, Symbol)],
    val publics : Seq[(String, Symbol)],
    localScope : Scope[String, Symbol],
    varInitializers : Seq[(Symbol, SExpression[BaseItem])],
    allFunctions : Seq[(Symbol, Seq[SExpression[BaseItem]], Seq[SExpression[BaseItem]])],
    allVars : Seq[Symbol],
    module : File) {

  def compile(rs : Scope[String, Symbol], trace : HostTrace)
      : ((Set[Symbol], Seq[(Symbol, FunctionBody)], Seq[Statement])) = {

    val modScope = Scope.chain(rs, localScope)
    val mc = new ExprComp(module, trace, modScope)

    val stmts = varInitializers.map(x ⇒
      Model.assign(x._1.resolve.asInstanceOf[LeftValue], mc.compile(x._2)))

    val globIdMap = globals.map(x ⇒ (x._2, x._1)).toMap[Symbol, String]
    val globIds = new ArrayBuffer[String]
    varInitializers.foreach(x ⇒ globIdMap.get(x._1) match {
        case None ⇒  ()
        case Some(x) ⇒  globIds += x
      })

    (allVars.toSet,
      allFunctions.map(x ⇒
        (x._1, FuncComp.compFunction(module, modScope, x._2, x._3, trace))),
      stmts)
  }
}



/** Utilities for the premodule. */
final object Premodule {

  /** Precompiles items into a new premodule. Returns a new
   * module, list of duplicate declarations and list of bad declarations. */
  def precompile(trace : HostTrace, module : File, elts : SList[BaseItem]) : Premodule = {
    val pmb = new PremoduleBuilder(trace, module)
    elts.items.foreach(pmb.acceptDef)
    pmb.end(elts)
  }
}
