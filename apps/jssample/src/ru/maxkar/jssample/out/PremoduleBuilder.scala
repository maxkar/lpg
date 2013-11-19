package ru.maxkar.jssample.out

import ru.maxkar.jssample.msg.HostTrace

import ru.maxkar.jssample.ns._
import ru.maxkar.jssample.att._
import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.lispy.parser.Input

import ru.maxkar.scoping.simple._


import scala.collection.mutable.ArrayBuffer

private[out] final class PremoduleBuilder(trace : HostTrace, module : java.io.File, id : Seq[String]) {
  import CompilerUtil._


  /** Collected names. */
  private val names = ScopeBuilder.withCallback[String, Symbol](
    (k, s1, s2) ⇒
      trace.duplicateDeclaration(k,
        s1.declaration.offset,
        s2.declaration.offset))


  /** Public scope. */
  private val pubScope = ScopeBuilder.withCallback[String, Symbol](
    (k, s1, s2) ⇒ ())


  /** Global names. */
  private val globals = new ArrayBuffer[(String, Symbol)]


  /** Public names. */
  private val publics = new ArrayBuffer[(String, Symbol)]


  /** Global variables. */
  private val allVars = new ArrayBuffer[Symbol]


  /** Variable initializers. */
  private val varInitializers = new ArrayBuffer[(Symbol, SExpression[BaseItem])]


  /** Global functions. */
  private val allFunctions =
    new ArrayBuffer[
      (Symbol, Seq[SExpression[BaseItem]], Seq[SExpression[BaseItem]])]



  /** Resolves an access attribute, if it is present. */
  private def accOf(item : SExpression[BaseItem]) : Option[Access] = {
    val atts = item.atts
    val items = atts.allValues(Access.ATTR)
    if (items.isEmpty)
      None
    else if (items.size == 1)
      Some(items.head)
    else {
      trace.duplicateAccessDefinition(item)
      None
    }
  }



  /** Calculates an effective access. */
  private def effectiveAcc(dflt : Access, defs : SExpression[BaseItem]*) : Access = {
    val itr = defs.iterator
    while (itr.hasNext)
      accOf(itr.next) match {
        case Some(x) ⇒  return x
        case None ⇒  ()
      }

    dflt
  }


  /** Creates a new id using a list of access definers. */
  private def mkSymbol(name : String, base : SExpression[BaseItem],
        accDefault : Access, accDefiners : SExpression[BaseItem]*) : Symbol = {

    val res = new Symbol(new ModuleHost(module, locOf(base)))
    names.offer(name, res)

    val acc = effectiveAcc(accDefault, accDefiners :_*)
    acc match {
      case Export ⇒
        publics += ((name, res))
        pubScope.offer(name, res)
        globals += ((name, res))
      case Public ⇒
        publics += ((name, res))
        pubScope.offer(name, res)
      case Private ⇒
    }

    res
  }


  private def acceptVar(item : SExpression[BaseItem], scope : SExpression[BaseItem]) : Unit = {
    item match {
      case SLeaf(BaseId(x), _) ⇒
        allVars += mkSymbol(x, item, Private, item, scope)
      case SList(Seq(a@SLeaf(BaseId(x), _), iv), _) ⇒
        val vid = mkSymbol(x, a, Private, item, scope)
        allVars += vid
        varInitializers += ((vid, iv))
      case _ ⇒
        trace.mailformedDeclaration(item)
    }
  }


  def acceptDef(item : SExpression[BaseItem]) : Unit = {
    item match {
      case SList(Seq(SLeaf(BaseId("var"), _), tl@_*), _) ⇒
        tl.foreach(acceptVar(_, item))
      case SList(Seq(
          SLeaf(BaseId("def"), _),
          ndef@SLeaf(BaseId(name), _),
          SList(args, _),
          tail@_*), _) ⇒
        val fid = mkSymbol(name, ndef, Public, item)
        allFunctions += ((fid, args, tail))
      case _ ⇒
        trace.mailformedDeclaration(item)
    }
  }


  /** Ends a building. */
  def end(e : SExpression[BaseItem]) : Premodule =
    new Premodule(id, globals, publics, pubScope.scope,
      names.scope, varInitializers,
      allFunctions, allVars, e, module)
}
