package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/**
 * Negative number expression. Writes additional space in
 * unsafe context.
 * @param num textual number representation.
 */
private[model] final class NegativeNumberExpression(num : String)
    extends TextExpression(num) {

  override private[model] def writeExpressionAfterMinus(
      writer : CompactContext) : Unit = {
    writer.writeChar(' ')
    writeExpression(writer)
  }
}
