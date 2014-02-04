package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Marker interface for objects which can create
 * values.
 */
class NonprimitiveExpression(
    priority : Int,
    writer : CompactContext ⇒ Unit,
    canStartStatement : Boolean = true
  ) extends Expression(
    priority,
    writer,
    canStartStatement = canStartStatement) {

}
