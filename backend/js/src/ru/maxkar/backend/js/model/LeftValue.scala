package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Marker for a left-value expressions. */
class LeftValue(
    priority : Int,
    writer : CompactContext ⇒ Unit,
    canStartStatement : Boolean = true)
  extends NonprimitiveExpression(
    priority,
    writer,
    canStartStatement) {
}
