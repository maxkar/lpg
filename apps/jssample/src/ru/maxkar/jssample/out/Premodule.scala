package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.TextPosition

import ru.maxkar.backend.js.model._

import ru.maxkar.scoping.simple._

import java.io.File

import scala.collection.mutable.ArrayBuffer


/**
 * Class, which can be turned into a module by
 * providing an "external context"
 */
final class Premodule(
    val scope : Scope[String, ToplevelItem],
    val defKeys : java.util.Map[String, ToplevelItem],
    entry : SExpression[BaseItem]) {

  def compile(rs : Scope[String, ToplevelItem])
      : ((Set[String], Seq[(String, FunctionBody)], Seq[Statement]), Seq[Message]) = {

    val comp = new PremoduleCompiler(rs)
    comp.acceptTop(entry)
    comp.end
  }
}



/** Utilities for the premodule. */
final object Premodule {

  /** Precompiles items into a new premodule. Returns a new
   * module, list of duplicate declarations and list of bad declarations. */
  def precompile(host : File, elts : SExpression[BaseItem]) :
      (Premodule, Seq[Message]) = {

    val pmb = new PremoduleBuilder(host)
    pmb.acceptTop(elts)


    (pmb.end(elts), pmb.messages)
  }
}
