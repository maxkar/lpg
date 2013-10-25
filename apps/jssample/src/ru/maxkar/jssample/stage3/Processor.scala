package ru.maxkar.jssample.stage3

import ru.maxkar.jssample.{stage1 ⇒ S1}

import ru.maxkar.hunk.Hunk
import ru.maxkar.hunk.Hunk._

import ru.maxkar.lispy._

import ru.maxkar.jssample.ns._
import ru.maxkar.scoping._

import java.util.concurrent.Executor

/** Stage-3 (resolution phase) processor. */
final class Processor(namespaces : Map[Seq[String], Namespace[Reference]])
    (implicit executor : Executor) {

  def process(item : S1.Result) : Hunk[Result] = exec {
    null
  }
}
