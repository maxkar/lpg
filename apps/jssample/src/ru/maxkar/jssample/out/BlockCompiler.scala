package ru.maxkar.jssample.out

import ru.maxkar.backend.js.model._
import ru.maxkar.scoping.simple._

import scala.collection.mutable.ArrayBuffer

/** Compiler for the statement block. */
trait BlockCompiler {


  /** Compiles statements and invokes a callback on the result. */
  def compileStatements(ctx : LocalContext, cb : Statement ⇒  Unit) : Unit


  /** Compiles this block into a sequence of statements. */
  def compileToSeq(ctx : LocalContext): Seq[Statement] = {
    val res = new ArrayBuffer[Statement]
    compileStatements(ctx, res.+=)
    res
  }
}
