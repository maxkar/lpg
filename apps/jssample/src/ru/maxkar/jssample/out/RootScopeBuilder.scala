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

/** Builder for the "root level" scope. Root level scope
 * is scope at the function level. Source files may have nested
 * (implicit) blocks, but javascript treats all variables as
 * declared at a "top level" so we need to lift definition to that
 * level.
 */
final class RootScopeBuilder(host : File) {
  import CompilerUtil._


  /** All arguments. */
  private var args = new ArrayBuffer[Symbol]
  /** All locals. */
  private val locals = new ArrayBuffer[Symbol]
  /** All labels. */
  private val labels = new ArrayBuffer[Symbol]
  /** All local functions. */
  private val funcs = new ArrayBuffer[
    (Symbol, Boolean, Seq[SExpression[BaseItem]], Seq[SExpression[BaseItem]])]


  /** Creates a new argument variable for the given host. */
  def mkArg(item : SExpression[BaseItem]) : Symbol = {
    val res = new Symbol(new ModuleHost(host, locOf(item)))
    args += res
    res
  }


  /** Creates a new local variable for the given host. */
  def mkVar(item : SExpression[BaseItem]) : Symbol = {
    val res = new Symbol(new ModuleHost(host, locOf(item)))
    locals += res
    res
  }


  /** Creates a new automatical variable. */
  def mkAutoVar(item : SExpression[BaseItem]) : Symbol =
    new Symbol(new ModuleHost(host, locOf(item)))


  /** Creates a new local label. */
  def mkLabel(item : SExpression[BaseItem]) : Symbol = {
    val res = new Symbol(new ModuleHost(host, locOf(item)))
    labels += res
    res
  }


  /** Creates a new local function. */
  def mkFunction(
        defn : SExpression[BaseItem],
        isvaarg : Boolean,
        args : Seq[SExpression[BaseItem]],
        body : Seq[SExpression[BaseItem]])
      : Symbol = {
    val res = new Symbol(new ModuleHost(host, locOf(defn)))
    funcs += ((res, isvaarg, args, body))
    res
  }

  def getArgs() : Seq[Symbol] = args
  def getVars() : Seq[Symbol] = locals
  def getLabels() : Seq[Symbol] = labels
  def getFuncs() : Seq[(Symbol, Boolean, Seq[SExpression[BaseItem]], Seq[SExpression[BaseItem]])] =
    funcs
}
