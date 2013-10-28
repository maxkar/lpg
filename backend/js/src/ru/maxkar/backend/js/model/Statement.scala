package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Statement representation. */
trait Statement {
  /** Writes statement into a given context. */
  private[model] def writeStatement(ctx : CompactContext) : Unit
}
