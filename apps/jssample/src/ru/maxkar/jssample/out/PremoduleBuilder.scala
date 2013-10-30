package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.lispy.parser.Input

import ru.maxkar.scoping.simple._

import java.io.File

import scala.collection.mutable.ArrayBuffer

private[out] final class PremoduleBuilder(host : File) {
  private val defmap = new ScopeBuilder[String, ToplevelItem]
  private val baddecl = new ArrayBuffer[Message]

  private def locOf(x : SExpression[BaseItem]) : TextPosition = {
    val slist = x.atts.allValues(Input.textPosition)
    if (slist.isEmpty)
      null
    else
      slist.iterator.next
  }


  private def acceptVar(item : SExpression[BaseItem]) : Unit = {
    item match {
      case SLeaf(BaseId(x), _) ⇒ defmap.offer(x, new TIVar(
        new ModuleHost(host, locOf(item)), None))
      case SList(Seq(a@SLeaf(BaseId(x), _), iv), _) ⇒
        defmap.offer(x, new TIVar(
          new ModuleHost(host, locOf(a)), Some(iv)))
      case _ ⇒
        baddecl += BadDeclaration(host, locOf(item))
    }
  }


  private def acceptFunction(pos : TextPosition, item : Seq[SExpression[BaseItem]]) : Unit = {
    item match {
      case Seq(
          hd@SLeaf(BaseId(x), _),
          SList(args, _),
          tail@_*) ⇒
        defmap.offer(x, new TIFunction(
          new ModuleHost(host, locOf(hd)), args, tail))
      case _ ⇒
        baddecl += BadDeclaration(host, pos)
    }
  }


  private def acceptDef(item : SExpression[BaseItem]) : Unit = {
    val hleaf = item.unHeadLeaf
    if (hleaf == null) {
      baddecl += BadDeclaration(host, locOf(item))
      return
    }

    val idn = hleaf.unId
    if (idn == null) {
      baddecl += BadDeclaration(host, locOf(item))
      return
    }


    idn match {
      case "var" ⇒ item.tail.foreach(acceptVar)
      case "def" ⇒ acceptFunction(locOf(item), item.tail)
      case _ ⇒
        baddecl += BadDeclaration(host, locOf(item))
    }
  }


  def acceptTop(expr : SExpression[BaseItem]) : Unit = {
    expr match {
      case SList(items, _) ⇒
        items.foreach(acceptDef)
      case _ ⇒
        baddecl += BadDeclaration(host, locOf(expr))
    }
  }


  def messages() : Seq[Message] =
    baddecl.toSeq  ++
    defmap.duplicates.map(x ⇒
      DuplicateDeclaration(host, x._1, x._2.declarationHost.offset, x._3.declarationHost.offset))

  /** Ends a building. */
  def end(e : SExpression[BaseItem]) : Premodule =
    new Premodule(defmap.scope, defmap.entries, e)
}
