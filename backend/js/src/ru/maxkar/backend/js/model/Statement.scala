package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Statement representation. */
class Statement private[model](private[model] val stmtWriter : CompactContext ⇒ Unit) {
  /** Writes statement into a given context. */
  private[model] def writeStatement(ctx : CompactContext) : Unit = stmtWriter(ctx)
}
