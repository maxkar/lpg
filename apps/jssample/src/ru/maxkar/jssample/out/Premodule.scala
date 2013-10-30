package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.TextPosition

import ru.maxkar.scoping.simple._

import java.io.File

import scala.collection.mutable.ArrayBuffer


/**
 * Class, which can be turned into a module by
 * providing an "external context"
 */
final class Premodule {
}


/** Utilities for the premodule. */
final object Premodule {

  /** Precompiles items into a new premodule. Returns a new
   * module, list of duplicate declarations and list of bad declarations. */
  def precompile(host : File, elts : SExpression[BaseItem]) :
      (Premodule, Seq[Message]) = {

    val pmb = new PremoduleBuilder(host)
    pmb.acceptTop(elts)


    (new Premodule, pmb.messages)
  }
}
