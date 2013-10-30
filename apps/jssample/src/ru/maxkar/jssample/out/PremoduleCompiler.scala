package ru.maxkar.jssample.out

import ru.maxkar.jssample.ns._
import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.TextPosition

import ru.maxkar.backend.js.model._

import ru.maxkar.scoping.simple._

import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

/** Compiler for the premodule. */
private[out] final class PremoduleCompiler(rs : Scope[String, ToplevelItem]) {
  private val baddecl = new ArrayBuffer[Message]
  private val funcmap = new HashMap[String, FunctionBody]
  private val stmts = new ArrayBuffer[Statement]
  private val globs = new HashSet[String]


  private def acceptVar(item : SExpression[BaseItem]) : Unit = {
    item match {
      case SLeaf(BaseId(x), _) ⇒
        globs += x
      case SList(Seq(a@SLeaf(BaseId(x), _), iv), _) ⇒
        globs += x
      case _ ⇒ ()
    }
  }


  private def acceptFunction(item : Seq[SExpression[BaseItem]]) : Unit = {
    item match {
      case Seq(
          hd@SLeaf(BaseId(x), _),
          SList(args, _),
          tail@_*) ⇒
        ()
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
