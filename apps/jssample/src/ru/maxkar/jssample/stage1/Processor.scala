package ru.maxkar.jssample.stage1

import ru.maxkar.jssample._

import ru.maxkar.lispy._
import ru.maxkar.scoping._

import ru.maxkar.hunk._
import ru.maxkar.hunk.Hunk._

import java.util.concurrent.Executor

import ru.maxkar.jssample.{stage0 ⇒ S0}

/** Processor object for the first stage. */
final class Processor(implicit exec : Executor) {
  def proc(input : S0.Item) : Hunk[S0.Item, Trace] = execT (tr ⇒ {
    val b = Declarations.liftDeclarations(input.body,
      new Declarations.DeclarationCallback {
        def duplicateDeclaration(name: String, first: Attributes, next: Attributes): Unit = {
          tr(TrDuplicateDeclaration(input.source, name, first, next))
        }
      })
    S0.Item(input.source, input.path, b)
  })
}
