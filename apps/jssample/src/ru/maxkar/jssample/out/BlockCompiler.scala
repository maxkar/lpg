package ru.maxkar.jssample.out

import ru.maxkar.backend.js.model._
import ru.maxkar.scoping.simple._

import scala.collection.mutable.ArrayBuffer

/** Compiler for the statement block. */
trait BlockCompiler {


  /** Compiles statements and invokes a callback on the result. */
  def compileStatements(
      vars : Scope[String, ToplevelItem],
      labels : Scope[String, ToplevelItem],
      baddecl : ArrayBuffer[Message],
      cb : Statement ⇒  Unit) : Unit


  /** Compiles this block into a sequence of statements. */
  def compileToSeq(
        vars : Scope[String, ToplevelItem],
        labels : Scope[String, ToplevelItem],
        baddecl : ArrayBuffer[Message]):
      Seq[Statement] = {
    val res = new ArrayBuffer[Statement]
    compileStatements(vars, labels, baddecl, res.+=)
    res
  }
}
