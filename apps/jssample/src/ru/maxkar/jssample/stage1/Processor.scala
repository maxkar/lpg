package ru.maxkar.jssample.stage1

import ru.maxkar.jssample._

import ru.maxkar.lispy._
import ru.maxkar.scoping._

import ru.maxkar.hunk._
import ru.maxkar.hunk.Hunk._

import java.util.concurrent.Executor

import ru.maxkar.jssample.{stage0 ⇒ S0}

import scala.collection.mutable.ArrayBuffer

/** Processor object for the first stage. */
final class Processor(implicit executor : Executor) {
  def proc(input : S0.Item) : Hunk[Result] = calc {
    val (b, dupes) = Declarations.liftDeclarations(input.body)
    new Result(input.source, input.path, b, new Anamnesis1(dupes))
  }
}
