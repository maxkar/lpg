package ru.maxkar.jssample.out

import ru.maxkar.jssample.msg.HostTrace

import ru.maxkar.jssample.ns._
import ru.maxkar.jssample.att._
import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.lispy.parser.Input

import ru.maxkar.scoping.simple._

import ru.maxkar.jssample.doc._


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


  /** Global variables. */
  private val allVars = new ArrayBuffer[Symbol]


  /** Variable initializers. */
  private val varInitializers = new ArrayBuffer[(Symbol, SExpression[BaseItem])]


  /** Variable documentation. */
  private val varDoc = new ArrayBuffer[VarDoc]


  /** Function documentation. */
  private val funDoc = new ArrayBuffer[FunDoc]


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


  /** Exports an item if it is requested by the attribute value.
   * @return name of the exported item (if possible).
   */
  private def exportIfNeeded(atts : SExpression[BaseItem], symbol : Symbol, defName : String) : Option[String] = {
    val attr = atts.atts.allValues(Export.ATTR)

    if (attr.isEmpty)
      return None
    if (attr.size > 1) {
      trace.duplicateExportAttribute(atts)
      return None
    }

    attr.head match {
      case None ⇒
        globals += ((defName, symbol))
        Some(defName)
      case Some(name) ⇒
        globals += ((name, symbol))
        Some(name)
    }
  }


  /** Creates a new id using a list of access definers. */
  private def mkSymbol(name : String, base : SExpression[BaseItem],
        accDefault : Access, accDefiners : SExpression[BaseItem]*) : (Symbol, Option[String], Access) = {

    val res = new Symbol(new ModuleHost(module, locOf(base)))
    names.offer(name, res)

    val acc = effectiveAcc(accDefault, accDefiners :_*)
    acc match {
      case Public ⇒ pubScope.offer(name, res)
      case Private ⇒
    }

    val extName = exportIfNeeded(base, res, name)

    (res, extName, acc)
  }


  /** Calculates documentation of the item. */
  private def docOf(item : SExpression[BaseItem]): DocBody = {
    val att = item.atts.allValues(Doc.ATTR)
    if (att.isEmpty)
      DocBody.text("")
    else
      att.head
  }


  /** Attempts to document a variable. */
  private def docVar(
        item : SExpression[BaseItem],
        name : String,
        externName : Option[String],
        acc : Access) : Unit = {
    varDoc += new VarDoc(name, externName, acc == Public, docOf(item))
  }


  private def acceptVar(item : SExpression[BaseItem], scope : SExpression[BaseItem]) : Unit = {
    item match {
      case SLeaf(BaseId(x), _) ⇒
        val (sym, extName, acc) = mkSymbol(x, item, Private, item, scope)
        docVar(item, x, extName, acc)
        allVars += sym
      case SList(Seq(a@SLeaf(BaseId(x), _), iv), _) ⇒
        val (vid, extName, acc) = mkSymbol(x, a, Private, item, scope)
        docVar(a, x, extName, acc)
        allVars += vid
        varInitializers += ((vid, iv))
      case _ ⇒
        trace.mailformedDeclaration(item)
    }
  }


  private def fnArgDoc(item : SExpression[BaseItem]) : ArgDoc = {
    item match {
      case SLeaf(BaseId(name), _) ⇒ new ArgDoc(name, docOf(item))
      case _ ⇒ new ArgDoc("???", DocBody.text("???"))
    }
  }


  def acceptDef(item : SExpression[BaseItem]) : Unit = {
    item match {
      case SList(Seq(SLeaf(BaseId("var"), _), tl@_*), _) ⇒
        tl.foreach(acceptVar(_, item))
      case SList(Seq(
          dfn@SLeaf(BaseId("def"), _),
          ndef@SLeaf(BaseId(name), _),
          alist@SList(args, _),
          tail@_*), _) ⇒
        val (fid, extName, acc) = mkSymbol(name, ndef, Public, item)
        allFunctions += ((fid, args, tail))

        funDoc += new FunDoc(name, extName, acc == Public,
           docOf(dfn),
           args.map(fnArgDoc),
           docOf(alist))
      case _ ⇒
        trace.mailformedDeclaration(item)
    }
  }


  /** Ends a building. */
  def end(e : SExpression[BaseItem]) : Premodule = {
    val moddoc = e.atts.allValues(Doc.ATTR)

    new Premodule(id, globals, pubScope.scope,
      if (moddoc.isEmpty) None else Some(moddoc.head),
      varDoc, funDoc,
      names.scope, varInitializers,
      allFunctions, allVars, e, module)
  }
}
