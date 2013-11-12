package ru.maxkar.jssample.out

import ru.maxkar.jssample.msg.HostTrace

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.lispy.parser.Input

import ru.maxkar.scoping.simple._


import scala.collection.mutable.ArrayBuffer

private[out] final class PremoduleBuilder(trace : HostTrace, module : java.io.File) {
  import CompilerUtil._


  /** Collected names. */
  private val names = ScopeBuilder.withCallback[String, Symbol](
    (k, s1, s2) ⇒
      trace.duplicateDeclaration(k,
        s1.declaration.offset,
        s2.declaration.offset))


  /** Global names. */
  private val globals = new ArrayBuffer[(String, GlobalSymbol)]


  /** Global variables. */
  private val globalVars = new ArrayBuffer[String]


  /** Variable initializers. */
  private val varInitializers = new ArrayBuffer[(Symbol, SExpression[BaseItem])]


  /** Global functions. */
  private val globalFunctions =
    new ArrayBuffer[
      (String, Seq[SExpression[BaseItem]], Seq[SExpression[BaseItem]])]


  /** Creates a new global item. */
  private def glob(name : String, base : SExpression[BaseItem]) : GlobalSymbol = {
    val res = new GlobalSymbol(new ModuleHost(module, locOf(base)), name)
    names.offer(name, res)
    globals += ((name, res))
    res
  }


  private def acceptVar(item : SExpression[BaseItem]) : Unit = {
    item match {
      case SLeaf(BaseId(x), _) ⇒
        globalVars += x
        glob(x, item)
      case SList(Seq(a@SLeaf(BaseId(x), _), iv), _) ⇒
        globalVars += x
        varInitializers += ((glob(x, a), iv))
      case _ ⇒
        trace.mailformedDeclaration(item)
    }
  }


  def acceptDef(item : SExpression[BaseItem]) : Unit = {
    item match {
      case SList(Seq(SLeaf(BaseId("var"), _), tl@_*), _) ⇒
        tl.foreach(acceptVar)
      case SList(Seq(
          SLeaf(BaseId("def"), _),
          ndef@SLeaf(BaseId(name), _),
          SList(args, _),
          tail@_*), _) ⇒
        glob(name, ndef)
        globalFunctions += ((name, args, tail))
      case _ ⇒
        trace.mailformedDeclaration(item)
    }
  }


  /** Ends a building. */
  def end(e : SExpression[BaseItem]) : Premodule =
    new Premodule(globals, names.scope, varInitializers,
      globalFunctions, globalVars, module)
}
